;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Sep 10 18:03:52 2003
;;;; Contains: Simple randon form generator/tester

(in-package :cl-test)

(compile-and-load "random-aux.lsp")

;;;
;;; This file contains a routine for generating random legal Common Lisp functions
;;; for differential testing.
;;;
;;; To run the random tests by themselves, start a lisp in the ansi-tests directory
;;; and do the following:
;;;   (load "gclload1.lsp")
;;;   (compile-and-load "random-int-form.lsp")
;;;   (in-package :cl-test)
;;;   (let ((*random-state* (make-random-state t)))
;;;      (test-random-integer-forms 100 4 10000)) ;; or other parameters
;;;
;;; If a test breaks during testing the variables *optimized-fn-src*,
;;; *unoptimized-fn-src*, and *int-form-vals* can be used to get the source
;;; of the optimized/unoptimized lambda forms being compiled, and the arguments
;;; on which they are called.
;;;
;;; If a difference is found between optimized/unoptimized functions the forms,
;;; values, and results are collected.  A list of all these discrepancies is returned
;;; after testing finishes (assuming nothing breaks).
;;;
;;; The rctest/ subdirectory contains fragments of a more OO random form generator
;;; that will eventually replace this preliminary effort.
;;;

(declaim (special *optimized-fn-src* *unoptimized-fn-src* *int-form-vals*
		  *opt-result* *unopt-result*))

(defvar *random-int-form-blocks* nil)

(defun test-random-integer-forms (size nvars n)

  "Generate random integer forms of size SIZE with NVARS variables.
   Do this N times, returning all those on which a discrepancy
   is found between optimized and nonoptimize, notinlined code."

  (assert (integerp nvars))
  (assert (<= 1 nvars 26))
  (assert (and (integerp n) (plusp n)))
  (assert (and (integerp n) (plusp size)))
  
  (loop for i from 1 to n
	do (princ ".") (finish-output *standard-output*)
	nconc (test-random-integer-form size nvars)))

(defun test-random-integer-form
  (size nvars &aux (vars (subseq '(a b c d e f g h i j k l m
				     n o p q r s t u v w x y z) 0 nvars)))
  (let* ((form (make-random-integer-form size vars))
	 (var-ranges (mapcar #'make-random-integer-range vars))
	 (var-types (mapcar #'(lambda (range)
				(let ((lo (car range))
				      (hi (cadr range)))
				  (assert (>= hi lo))
				  `(integer ,lo ,hi)))
			    var-ranges))
	 (vals-list
	  (loop repeat 20
		collect
		(mapcar #'(lambda (range)
			    (let ((lo (car range))
				  (hi (cadr range)))
			      (random-from-interval (1+ hi) lo)))
			var-ranges))))
    (test-int-form form vars var-types vals-list)))

(defun fn-symbols-in-form (form)
  "Return a list of the distinct standardized lisp function
   symbols occuring ing FORM.  These are used to generate a NOTINLINE
   declaration for the unoptimized form."
  (intersection
   (remove-duplicates (fn-symbols-in-form* form) :test #'eq)
   *cl-function-or-accessor-symbols*))

(defun fn-symbols-in-form* (form)
  (when (consp form)
    (if (symbolp (car form))
	(cons (car form) (mapcan #'fn-symbols-in-form* (cdr form)))
      (mapcan #'fn-symbols-in-form* form))))

(defun make-random-integer-range (var)
  "Generate a list (LO HI) of integers, LO <= HI.  This is used
   for generating integer types."
  (declare (ignore var))
  (flet ((%r () (let ((r (ash 1 (1+ (random 35)))))
		  (- (random r) (floor (/ r 2))))))
    (let ((x (%r))
	  (y (%r)))
      (list (min x y) (max x y)))))

(defun make-random-integer-form (size vars)
  "Generate a random legal lisp form of size SIZE (roughly).  VARS
   is a list of variable symbols that contain integers."
  (if (<= size 1)
      ;; Leaf node -- generate a variable or constant
      (if (= (random 2) 0)
	  (let ((r (ash 1 (1+ (random 32)))))
	    (- (random r) (floor (/ r 2))))
	(random-from-seq vars))
    ;; (> size 1)
    (rcase
     ;; Unary ops
     (40
      (let ((op (random-from-seq '(- abs abs signum signum 1+ 1- identity
				     integer-length logcount))))
	`(,op ,(make-random-integer-form (1- size) vars))))
     ;; Binary op
     (80
      (let* ((op (random-from-seq
		  '(+ - * logand min max min max ;; gcd lcm
		      #-:allegro logandc1  ;; bug in ACL 6.2 makes this cause crashes
		      logandc2 logeqv logior lognand lognor logorc1
		      logorc2 logxor))))
	(destructuring-bind (leftsize rightsize)
	    (random-partition (1- size) 2)
	  (let ((e1 (make-random-integer-form leftsize vars))
		(e2 (make-random-integer-form rightsize vars)))
	    `(,op ,e1 ,e2)))))
     ;; conditionals
     (20
      (let* ((cond-size (random (max 1 (floor size 2))))
	     (then-size (random (- size cond-size)))
	     (else-size (- size 1 cond-size then-size))
	     (pred (make-random-pred-form cond-size vars))
	     (then-part (make-random-integer-form then-size vars))
	     (else-part (make-random-integer-form else-size vars)))
	`(if ,pred ,then-part ,else-part)))
     (10
      (destructuring-bind (s1 s2 s3) (random-partition (1- size) 3)
	`(,(random-from-seq '(deposit-field dpb))
	  ,(make-random-integer-form s1 vars)
	  ,(make-random-byte-spec-form s2 vars)
	  ,(make-random-integer-form s3 vars))))
     (10
      (destructuring-bind (s1 s2) (random-partition (1- size) 2)
	  `(,(random-from-seq '(ldb mask-field))
	    ,(make-random-byte-spec-form s1 vars)
	    ,(make-random-integer-form s2 vars))))
     (20
      (destructuring-bind (s1 s2) (random-partition (1- size) 2)
	(let* ((var (random-from-seq #(v1 v2 v3 v4 v5 v6 v7 v8 v9 v10)))
	       (e1 (make-random-integer-form s1 vars))
	       (e2 (make-random-integer-form s2 (cons var vars))))
	  ;; for now, avoid shadowing
	  (if (member var vars)
	      (make-random-integer-form size vars)
	    `(let ((,var ,e1)) ,e2)))))
     (10
      (let* ((name (random-from-seq #(b1 b2 b3 b4 b5 b6 b7 b8)))
	     (*random-int-form-blocks* (adjoin name *random-int-form-blocks*)))
	`(block ,name ,(make-random-integer-form (1- size) vars))))
     (1
      (if *random-int-form-blocks*
	  (let ((name (random-from-seq *random-int-form-blocks*))
		(form (make-random-integer-form (1- size) vars)))
	    `(return-from ,name ,form))
	;; No blocks -- try again
	(make-random-integer-form size vars)))

     (5
      (if *random-int-form-blocks*
	  (destructuring-bind (s1 s2 s3) (random-partition (1- size) 3)
	    (let ((name (random-from-seq *random-int-form-blocks*))
		  (pred (make-random-pred-form s1 vars))
		  (then (make-random-integer-form s2 vars))
		  (else (make-random-integer-form s3 vars)))
	      `(if ,pred (return-from ,name ,then) ,else)))
	;; No blocks -- try again
	(make-random-integer-form size vars)))
		
     )))

(defun make-random-pred-form (size vars)
  (if (<= size 1)
      (rcase
	(1 (if (coin) t nil))
	(2
	 `(,(random-from-seq '(< <= = > >= /= eql equal))
	   ,(make-random-integer-form size vars)
	   ,(make-random-integer-form size vars))))
    (rcase
      (1 (if (coin) t nil))
      (1 `(not ,(make-random-pred-form (1- size) vars)))
      (2 (destructuring-bind (leftsize rightsize)
	     (random-partition (1- size) 2)
	   `(,(random-from-seq '(and or))
	     ,(make-random-pred-form leftsize vars)
	     ,(make-random-pred-form rightsize vars))))
      (1 (destructuring-bind (leftsize rightsize)
	     (random-partition (1- size) 2)
	   `(,(random-from-seq '(< <= > >= = /= eql equal))
	     ,(make-random-integer-form leftsize vars)
	     ,(make-random-integer-form rightsize vars))))
      (1 (let* ((cond-size (random (max 1 (floor size 2))))
		(then-size (random (- size cond-size)))
		(else-size (- size 1 cond-size then-size))
		(pred (make-random-pred-form cond-size vars))
		(then-part (make-random-pred-form then-size vars))
		(else-part (make-random-pred-form else-size vars)))
	   `(if ,pred ,then-part ,else-part)))
      (1 (destructuring-bind (s1 s2)
	     (random-partition (1- size) 2)
	   `(ldb-test ,(make-random-byte-spec-form s1 vars)
		      ,(make-random-integer-form s2 vars))))
      (1 (let ((index (random (1+ (random 35))))
	       (form (make-random-integer-form (1- size) vars)))
	   `(logbitp ,index ,form)))
      )))

(defun make-random-byte-spec-form (size vars)
  (declare (ignore size vars))
  (let* ((pform (random 33))
	 (sform (1+ (random 33))))
    `(byte ,sform ,pform)))
	
(defun random-partition (n p)
  "Partition n into p numbers, each >= 1.  Return list of numbers."
  (assert (<= 1 p))
  (cond
   ((= p 1) (list n))
   ((< n p) (make-list p :initial-element 1))
   (t
    (let ((n1 (1+ (random (floor n p)))))
      (cons n1 (random-partition (- n n1) (1- p)))))))

;;; Interface to the form pruner

(defun test-int-form (form vars var-types vals-list)
  ;; Try to compile FORM with associated VARS, and if it compiles
  ;; check for equality of the two compiled forms.
  ;; Return a non-nil list of details if a problem is found,
  ;; NIL otherwise.
  (let ((optimized-fn-src
	  `(lambda ,vars
	     (declare ,@(mapcar #'(lambda (tp var)
				    `(type ,tp ,var))
				var-types vars)
		      (ignorable ,@vars)
		      (optimize #+cmu (extensions:inhibit-warnings 3)
				(speed 3) (safety 1) (debug 1)))
	     ,form))
	 (unoptimized-fn-src
	  `(lambda ,vars
	     (declare (notinline ,@(fn-symbols-in-form form))
		      (optimize #+cmu (extensions:inhibit-warnings 3)
				(safety 3) (speed 0) (debug 3))
		      (ignorable ,@vars))
	     ,form)))
    (setq *int-form-vals* nil
	  *optimized-fn-src* optimized-fn-src
	  *unoptimized-fn-src* unoptimized-fn-src)
    (handler-bind
     (#+sbcl (sb-ext::compiler-note #'muffle-warning)
	     (warning #'muffle-warning)
	     (error #'(lambda (c)
			(format t "Compilation failure~%")
			(return-from test-int-form
			  (list (list :vars vars
				      :form form
				      :var-types var-types
				      :vals (first vals-list)
				      :compiler-condition c))))))
     (let ((optimized-compiled-fn (compile nil optimized-fn-src))
	   (unoptimized-compiled-fn (compile nil unoptimized-fn-src)))
       (dolist (vals vals-list)
	 (setq *int-form-vals* vals)
	 (let ((unopt-result (multiple-value-list
			    (apply unoptimized-compiled-fn vals)))
	       (opt-result (multiple-value-list
			      (apply optimized-compiled-fn vals))))
	   (if (equal opt-result unopt-result)
	       nil
	     (progn
	       (format t "Different results: ~A, ~A~%"
		       opt-result unopt-result)
	       (setq *opt-result* opt-result
		     *unopt-result* unopt-result)
	       (return (list (list :vars vars
				   :vals vals
				   :form form
				   :var-types var-types
				   :optimized-values opt-result
				   :unoptimized-values unopt-result
				   )))))))))))

(defun prune-int-form (form vars var-types vals-list)
  (flet ((%try-fn (new-form)
		  (when (test-int-form new-form vars var-types vals-list)
		    (setf form new-form)
		    (throw 'success nil))))
    (loop
     (catch 'success
       (prune form #'%try-fn)
       (return form)))))

(defun prune-results (result-list)
  (loop for result in result-list
	collect
	(let ((form (getf result :form))
	      (vars (getf result :vars))
	      (var-types (getf result :var-types))
	      (vals-list (list (getf result :vals)))
	      )
	  `(:vars ,vars
	    :var-types ,var-types
	    :vals ,(first vals-list)
	    :form ,(prune-int-form form vars var-types vals-list)))))

;;;
;;; The call (PRUNE form try-fn) attempts to simplify the lisp form
;;; so that it still satisfies TRY-FN.  The function TRY-FN should
;;; return if the substitution is a failure.  Otherwise, it should
;;; transfer control elsewhere via GO, THROW, etc.
;;;
;;; The return value of PRUNE should be ignored.
;;;
(defun prune (form try-fn)
  (flet ((try (x) (funcall try-fn x)))
    (when (consp form)
      (let ((op (car form))
	    (args (cdr form)))
	(case op
	 
	 ((signum integer-length logcount
		  gcd lcm logandc1 logandc2 lognand lognor logorc1 logorc2)
	  (mapc #'try args)
	  (try 0)
	  (try 1)
	  (try -1)
	  (prune-fn form try-fn))
	 
	 ((abs 1+ 1- identity)
	  (mapc #'try args)
	  (prune-fn form try-fn))
	 
	 ((- + * min max logand logior logxor logeqv and or not eq eql equal = < > <= >= /=)
	  (when (every #'constantp args)
	    (try (eval form)))
	  (mapc #'try args)
	  (prune-fn form try-fn))
	 
	 ((if)
	  (let (;; (pred (first args))
		(then (second args))
		(else (third args)))
	    (try then)
	    (try else)
	    (prune-fn form try-fn)))

	 ((byte)
	  (prune-fn form try-fn))

	 ((deposit-field dpb)
	  (try (first args))
	  (try (third args))
	  (prune-fn form try-fn))

	 ((ldb mask-field)
	  (try (second args))
	  (prune-fn form try-fn))

	 ((ldb-test)
	  (prune-fn form try-fn))

	 ((let let*)
	  (prune-let form try-fn))

	 ((block)
	  (let ((name (second form))
		(body (cddr form)))
	    (when (and body (null (cdr body)))
	      (when (not (find-in-tree name body))
		(try (first body)))
	      ;; Simplify the subexpression
	      (prune (first body)
		     #'(lambda (x)
			 (try `(block ,name ,x)))))))
	 
	 (otherwise
	  (prune-fn form try-fn))
	 )))))

(defun find-in-tree (value tree)
  (or (eql value tree)
      (and (consp tree)
	   (or (find-in-tree value (car tree))
	       (find-in-tree value (cdr tree))))))

(defun prune-fn (form try-fn)
  (let* ((i 0)
	 (op (car form))
	 (args (cdr form))
	 (len (length args)))
    (flet ((try-arg (x)
		    (funcall
		     try-fn
		     (cons op
			   (append (subseq args 0 i)
				   (list x)
				   (subseq args (1+ i)))))))
      (declare (dynamic-extent (function try-arg)))
      (loop while (< i len)
	    do (prune (elt args i) #'try-arg)
	       (incf i)))))

(defun prune-let (form try-fn)
  (let* ((op (car form))
	 (binding-list (cadr form))
	 (body (cddr form))
	 (body-len (length body))
	 (len (length binding-list)))
    (when (and (eql len 1)
	       (eql body-len 1)
	       (eql (caar binding-list) (car body)))
      (funcall try-fn (cadar binding-list)))
    (let ((i 0))
      (flet ((try-binding (x)
			  (funcall
			   try-fn
			   `(,op
			     (,@(subseq binding-list 0 i)
				,x
				,@(subseq binding-list (1+ i)))
			     ,@body))))
	(declare (dynamic-extent (function try-binding)))
	(loop while (< i len)
	      do (prune (elt binding-list i) #'try-binding)
	      (incf i))))
    (when body
      (unless binding-list
	(funcall try-fn (car (last body))))
      (when (and (car binding-list)
		 (not (cdr binding-list))
		 (not (cdr body)))
	(let ((binding (car binding-list)))
	  (unless (consp (second binding))
	    (funcall try-fn `(let ()
			       ,@(subst (cadr binding) (car binding) body))))))
      (prune (car (last body))
	     #'(lambda (form2)
		 (funcall try-fn
			  `(,@(butlast form) ,form2)))))))

