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

(defun do-random-type-prop-tests (&key operator
				       (minargs 2)
				       (maxargs minargs)
				       (arg-types nil)
				       (rest-type 'integer)
				       (reps 1000)
				       (enclosing-the nil))
  (assert (<= 1 minargs maxargs 20))
  (loop
   repeat reps
   do
   (let* ((param-names
	   '(p1 p2 p3 p4 p5 p6 p7 p8 p9 p10
	     p11 p12 p13 p14 p15 p16 p17 p18 p19 p20))
	  (nargs (+ minargs (random (- maxargs minargs -1))))
	  (types (append arg-types
			 (make-list (- nargs (length arg-types))
				    :initial-element rest-type)))
	  (vals (mapcar #'make-random-element-of-type types))
	  (is-var? (loop repeat (length vals) collect (coin)))
	  (params (loop for x in is-var?
			for p in param-names
			when x collect p))
	  (param-types (mapcar #'make-random-type-containing vals))
	  (type-decls (loop for x in is-var?
			    for p in param-names
			    for tp in param-types
			    when x
			    collect `(type ,tp ,p)))
	  (rval (cl:handler-bind
		 (#+sbcl (sb-ext::compiler-note #'muffle-warning)
			 (warning #'muffle-warning))
		 (eval (cons operator vals))))
	  (result-type (if (and enclosing-the (integerp rval))
			   (make-random-type-containing rval)
			 t))
	  (upgraded-result-type (upgraded-array-element-type `(eql ,rval)))
	  (expr `(,operator ,@(loop for x in is-var?
				    for v in vals
				    for p in param-names
				    collect (if x
						(rcase
						 (1 (let ((tp (make-random-type-containing v)))
						      `(the ,tp ,p)))
						 (1 p))
					      v))))
	  (form
	   `(lambda (r ,@params)
	      (declare (optimize speed (safety 1))
		       (type (simple-array ,upgraded-result-type nil) r)
		       ,@ type-decls)
	      (setf (aref r)
		    ,(if enclosing-the `(the ,result-type ,expr) expr))
	      (values))))
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
	    (r (make-array nil :element-type upgraded-result-type))
	    (result (progn (apply fn r param-vals) (aref r))))
       (setq *random-type-prop-result*
	     (list :upgraded-result-type upgraded-result-type
		   :form form
		   :vals vals
		   :result result
		   :rval rval))
       (unless (rt::equalp-with-case result rval)
	 (return *random-type-prop-result*))))))

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

(defun make-random-character ()
  (rcase
   (2 (random-from-seq +standard-chars+))
   (2 (or (code-char (random (min 256 char-code-limit)))
	  (make-random-character)))
   (1 (or (code-char (random (min (ash 1 16) char-code-limit)))
	  (make-random-character)))
   (1 (or (code-char (random (min (ash 1 24) char-code-limit)))
	  (make-random-character)))))

(defmethod make-random-type-containing ((val rational))
  (rcase
   (1 `(eql ,val))
   (1 (let* ((n1 (random 4))
	     (n2 (random 4))
	     (l1 (loop repeat n1 collect (make-random-element-of-type 'rational)))
	     (l2 (loop repeat n1 collect (make-random-element-of-type 'rational))))
	`(member ,@l1 ,val ,@l2)))
   (1 `(rational ,val))
   (1 `(rational * ,val))
   (1 (let ((v (make-random-element-of-type 'rational)))
	(if (<= v val)
	    `(rational ,v ,val)
	  `(rational ,val ,v))))
   ))

	  