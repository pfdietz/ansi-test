;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Dec 23 20:39:22 2004
;;;; Contains: Randomized tests of type propagation in the compiler

(in-package :cl-test)

(eval-when (load eval compile)
  (compile-and-load "random-int-form.lsp"))

(defvar *print-random-type-prop-input* nil)
(defparameter *random-type-prop-result* nil)

(defgeneric make-random-type-containing (val)
  (:documentation
   "Given a value, generate a random type that contains that value."))

(declaim (special *param-types* *params* *is-var?* *form*))

(defparameter *default-reps* 1000)
(defparameter *default-cell* nil)

(defun do-random-type-prop-tests
  (operator arg-types minargs
	    &key
	    (maxargs minargs)
	    (rest-type t)
	    (reps *default-reps*)
	    (enclosing-the nil)
	    (cell *default-cell*)
	    (test #'regression-test::equalp-with-case))
  (assert (<= 1 minargs maxargs 20))
  (dotimes (i reps)
    again
    (let* ((param-names
	   '(p1 p2 p3 p4 p5 p6 p7 p8 p9 p10
	     p11 p12 p13 p14 p15 p16 p17 p18 p19 p20))
	  (nargs (+ minargs (random (- maxargs minargs -1))))
	  (types (subseq 
		  (append arg-types
			  (make-list (max 0 (- nargs (length arg-types)))
				     :initial-element rest-type))
		  0 nargs))
	  ; (vals (mapcar #'make-random-element-of-type types))
	  (vals (setq *params*
		      (or (make-random-arguments types) (go again))))
	  (is-var? (loop repeat (length vals) collect (coin)))
	  (*is-var?* is-var?)
	  (params (loop for x in is-var?
			for p in param-names
			when x collect p))
	  (param-types (mapcar #'make-random-type-containing vals))
	  (*param-types* param-types)
	  (type-decls (loop for x in is-var?
			    for p in param-names
			    for tp in param-types
			    when x
			    collect `(type ,tp ,p)))
	  (rval (cl:handler-bind
		 (#+sbcl (sb-ext::compiler-note #'muffle-warning)
			 (warning #'muffle-warning))
		 (let ((eval-form (cons operator (loop for v in vals collect `(quote ,v)))))
		   ;; (print eval-form) (terpri)
		   ;; (dotimes (i 100) (eval eval-form))
		   (eval eval-form))))
	  (result-type (if (and enclosing-the (integerp rval))
			   (make-random-type-containing rval)
			 t))
	  (expr `(,operator ,@(loop for x in is-var?
				    for v in vals
				    for p in param-names
				    collect (if x
						(rcase
						 (1 (let ((tp (make-random-type-containing v)))
						      `(the ,tp ,p)))
						 (1 p))
					      (if (or (consp v)
						      (and (symbolp v) (not (or (keywordp v)
										(member v '(nil t))))))
						  `(quote ,v)
						  v)))))
	  (speed (random 4))
	  (space (random 4))
	  (safety #-allegro (random 4)
		  #+allegro (1+ (random 3)))
	  (debug (random 4))
	  (store-into-cell? (and cell (coin)))
	  (upgraded-result-type (and store-into-cell?
				     (upgraded-array-element-type `(eql ,rval))))
	  (form
	   `(lambda (,@(when store-into-cell? '(r)) ,@params)
	      (declare (optimize (speed ,speed) (safety ,safety) (debug ,debug) (space ,space))
		       ,@(when store-into-cell? `((type (simple-array ,upgraded-result-type nil) r)))
		       ,@ type-decls)
	      ,(let ((result-form
		      (if enclosing-the `(the ,result-type ,expr) expr)))
		 (if store-into-cell?
		     `(setf (aref r) ,result-form)
		   result-form))))
	  (*form* form))
     (when *print-random-type-prop-input*
       (let ((*print-pretty* t)
	     (*print-case* :downcase))
	 (print (list :form form :vals vals))))
     (finish-output)
     (let* ((param-vals (loop for x in is-var?
			      for v in vals
			      when x collect v))
	    (fn (cl:handler-bind
		 (#+sbcl (sb-ext::compiler-note #'muffle-warning)
			 (warning #'muffle-warning))
		 (compile nil form)))
	    (result (if store-into-cell?
			(let ((r (make-array nil :element-type upgraded-result-type)))
			  (apply fn r param-vals)
			  (aref r))
		      (apply fn param-vals))))
       (setq *random-type-prop-result*
	     (list :upgraded-result-type upgraded-result-type
		   :form form
		   :vals vals
		   :result result
		   :rval rval))
       (unless (funcall test result rval)
	 (return *random-type-prop-result*))))))

(defun make-random-arguments (types-or-funs)
  (let ((vals nil))
    (loop for type-or-fun in types-or-funs
	  for type = (or (typecase type-or-fun
			   ((and function (not symbol))
			    (apply type-or-fun vals))
			   (t type-or-fun))
			 (return-from make-random-arguments nil) ;; null type
			 )
	  for val = (make-random-element-of-type type)
	  do (setf vals (nconc vals (list val))))
    ;; (dolist (v vals) (describe v))
    vals))

;; Default method
(defmethod make-random-type-containing ((val t)) t)

(defmethod make-random-type-containing ((val standard-class))
  (random-from-seq #(standard-class class t)))

(defmethod make-random-type-containing ((val structure-class))
  (random-from-seq #(structure-class class t)))

(defmethod make-random-type-containing ((val integer))
  (rcase
   (2 (let ((tp (random-from-seq '(t number real rational))))
	(if #-allegro (coin) #+allegro nil
	    (find-class tp) tp)))
   (1 (random-from-seq '(integer signed-byte atom)))
   #- allegro (1 (find-class 'integer))
   (2 `(eql ,val))
   (2 (let* ((n1 (random 4))
	     (n2 (random 4))
	     (l1 (loop repeat n1 collect (make-random-integer)))
	     (l2 (loop repeat n2 collect (make-random-integer))))
	`(member ,@l1 ,val ,@l2)))
   (4 (let ((lo (abs (make-random-integer))))
	`(integer ,(- val lo))))
   (4 (let ((lo (abs (make-random-integer))))
	`(integer ,(- val lo) *)))
   (4 (let ((hi (abs (make-random-integer))))
	`(integer * ,(+ val hi))))
   (8 (let ((lo (abs (make-random-integer)))
	    (hi (abs (make-random-integer))))
	`(integer ,(- val lo) ,(+ val hi))))
   (2 (if (>= val 0) `unsigned-byte
	(make-random-type-containing val)))))

(defmethod make-random-type-containing ((val character))
  (rcase
   (1 `(eql ,val))
   (1 'character)
   (1 (if (typep val 'base-char) 'base-char (make-random-type-containing val)))
   (1 (if (typep val 'standard-char) 'standard-char (make-random-type-containing val)))
   (1 (if (typep val 'extended-char) 'extended-char (make-random-type-containing val)))
   (1 (let* ((n1 (random 4))
	     (n2 (random 4))
	     (l1 (loop repeat n1 collect (make-random-character)))
	     (l2 (loop repeat n2 collect (make-random-character))))
	`(member ,@l1 ,val ,@l2)))
   (1 (random-from-seq #(t atom)))))

(defmethod make-random-type-containing ((val symbol))
  (rcase
   (1 `(eql ,val))
   (3 'symbol)
   (1 (if (null val) 'null (make-random-type-containing val)))
   (1 (if (member val '(t nil)) 'boolean (make-random-type-containing val)))
   (1 (if (keywordp val) 'keyword (make-random-type-containing val)))
   (1 (let* ((n1 (random 4))
	     (n2 (random 4))
	     (l1 (loop repeat n1 collect (make-random-symbol)))
	     (l2 (loop repeat n2 collect (make-random-symbol))))
	`(member ,@l1 ,val ,@l2)))
   (1 (random-from-seq #(t atom)))))

(defmethod make-random-type-containing ((val rational))
  (rcase
   (1 `(eql ,val))
   (1 (let* ((n1 (random 4))
	     (n2 (random 4))
	     (l1 (loop repeat n1 collect (make-random-element-of-type 'rational)))
	     (l2 (loop repeat n2 collect (make-random-element-of-type 'rational))))
	`(member ,@l1 ,val ,@l2)))
   (1 `(rational ,val))
   (1 `(rational * ,val))
   (1 (let ((v (make-random-element-of-type 'rational)))
	(if (<= v val)
	    `(rational ,v ,val)
	  `(rational ,val ,v))))
   ))

(defmethod make-random-type-containing ((val float))
  (let ((names (loop for tp in '(short-float single-float double-float long-float)
		     when (typep val tp)
		     collect tp)))
    (rcase
     (1 `(eql ,val))
     (1 `(member ,val))
     (1 (random-from-seq names))
     (1 (if (>= val 0)
	    `(,(random-from-seq names) ,(float 0 val) ,val)
	  `(,(random-from-seq names) ,val ,(float 0 val)))))))

(defun make-random-array-dimension-spec (array dim-index)
  (assert (<= 0 dim-index))
  (assert (< dim-index (array-rank array)))
  (let ((dim (array-dimension array dim-index)))
    (rcase (1 '*) (1 dim))))

(defmethod make-random-type-containing ((val bit-vector))
  (rcase
   (1 (let ((root (if (and (coin)
			   (typep val 'simple-bit-vector))
		      'simple-bit-vector
		    'bit-vector)))
	(rcase (1 root)
	       (1 `(,root))
	       (3 `(,root ,(make-random-array-dimension-spec val 0))))))
   (2 (call-next-method))))

(defmethod make-random-type-containing ((val vector))
  (rcase
   (2 (let ((root 'vector)
	    (alt-root (if (and (coin) (simple-vector-p val)) 'simple-vector 'vector))
	    (etype (rcase (1 '*)
			  (1 (array-element-type val))
			  ;; Add rule for creating new element types?
			  )))
	(rcase (1 alt-root)
	       (1 `(,alt-root))
	       (1 `(,root ,etype))
	       (2 (if (and (simple-vector-p val) (coin))
		      `(simple-vector ,(make-random-array-dimension-spec val 0))
		    `(,root ,etype ,(make-random-array-dimension-spec val 0)))))))
   (3 (call-next-method))))

(defmethod make-random-type-containing ((val array))
  (let ((root (if (and (coin) (typep val 'simple-array)) 'simple-array 'array))
	(etype (rcase (1 (array-element-type val)) (1 '*)))
	(rank (array-rank val)))
    (rcase
     (1 root)
     (1 `(,root))
     (1 `(,root ,etype))
     (1 `(,root ,etype ,(loop for i below rank collect (make-random-array-dimension-spec val i))))
     (1 `(,root ,etype ,(loop for i below rank collect (array-dimension val i))))
     (1 `(,root ,etype ,rank)))))

(defmethod make-random-type-containing ((val string))
  (rcase
   (1 (let ((root (if (and (coin)
			   (typep val 'simple-string))
		      'simple-string
		    'string)))
	(rcase (1 root)
	       (1 `(,root))
	       (3 `(,root ,(make-random-array-dimension-spec val 0))))))
   (2 (call-next-method))))

(defmethod make-random-type-containing ((val cons))
  (rcase
   (2 'cons)
   (2 'list)
   (1 `(cons ,(make-random-type-containing (car val))))
   (1 `(cons ,(make-random-type-containing (car val))
	     ,(random-from-seq #(t *))))
   (2 `(cons ,(random-from-seq #(t *)) 
	     ,(make-random-type-containing (cdr val))))
   (2 `(cons ,(make-random-type-containing (car val))
	     ,(make-random-type-containing (cdr val))))
   (1 t)))

(defmethod make-random-type-containing ((val complex))
  (rcase
   (1 'complex)
   (1 'number)
   #-gcl (1 `(complex ,(upgraded-complex-part-type (type-of (realpart val)))))
   (1 `(eql ,val))))

(defmethod make-random-type-containing ((val generic-function))
  (rcase
   (1 'generic-function)
   (1 (call-next-method))))

(defmethod make-random-type-containing ((val function))
  (rcase
   (1 'function)
   (1 (if (typep val 'compiled-function)
	  'compiled-function
	'function))
   (1 t)))
