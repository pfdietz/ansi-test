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

(defun test-random-integer-forms
  (size nvars n &optional (*random-state* (make-random-state t)))

  "Generate random integer forms of size SIZE with NVARS variables.
   Do this N times, returning all those on which a discrepancy
   is found between optimized and nonoptimize, notinlined code."

  (assert (integerp nvars))
  (assert (<= 1 nvars 26))
  (assert (and (integerp n) (plusp n)))
  (assert (and (integerp n) (plusp size)))
  
  (loop for i from 1 to n
	do (when (= (mod i 100) 0)
	     (prin1 i) (princ " ") (finish-output *standard-output*))
	nconc (let ((result (test-random-integer-form size nvars)))
		(when result
		  (let ((*print-readably* t))
		    (terpri)
		    (print (car result))
		    (finish-output *standard-output*)))
		result)))

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

(defparameter *flet-names* nil)

(defun make-random-integer-form (size vars)
  "Generate a random legal lisp form of size SIZE (roughly).  VARS
   is a list of variable symbols that contain integers."
  (if (<= size 1)
      ;; Leaf node -- generate a variable, constant, or flet function call
      (rcase
       (10 (let ((r (ash 1 (1+ (random 32)))))
	     (- (random r) (floor (/ r 2)))))
       (9 (random-from-seq vars))
       (1 (if *flet-names* (list (random-from-seq *flet-names*))
	    (random-from-seq vars))))
    ;; (> size 1)
    (rcase
     ;; Unary ops
     (40
      (let ((op (random-from-seq '(- abs signum 1+ 1- identity progn floor
				     ceiling truncate round realpart imagpart
				     integer-length logcount values))))
	`(,op ,(make-random-integer-form (1- size) vars))))
     (2 `(isqrt (abs ,(make-random-integer-form (- size 2) vars))))
     (3
      (destructuring-bind (s1 s2)
	  (random-partition (- size 2) 2)
	`(ash ,(make-random-integer-form s1 vars)
	      (min ,(random 100)
		   ,(make-random-integer-form s2 vars)))))
     
     ;; binary floor, ceiling, truncate, round
     (4
      (let ((op (random-from-seq #(floor ceiling truncate round mod rem)))
	    (op2 (random-from-seq #(max min))))
	(destructuring-bind (s1 s2)
	  (random-partition (- size 2) 2)
	  `(,op  ,(make-random-integer-form s1 vars)
		 (,op2  ,(if (eq op2 'max)
			     (1+ (random 100))
			   (- (1+ (random 100))))
			,(make-random-integer-form s2 vars))))))
	    
     ;; Binary op
     (80
      (let* ((op (random-from-seq
		  '(+ - * * logand min max min max ;; gcd lcm
		      ;; #-:allegro
		      logandc1
		      logandc2 logeqv logior lognand lognor
		      ;; #-:allegro
		      logorc1
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
     ;; #-:allegro
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
     (4 `(let () ,(make-random-integer-form (1- size) vars)))
     (10
      (let* ((name (random-from-seq #(b1 b2 b3 b4 b5 b6 b7 b8)))
	     (*random-int-form-blocks* (adjoin name *random-int-form-blocks*)))
	`(block ,name ,(make-random-integer-form (1- size) vars))))
     (3 (make-random-integer-case-form size vars))
     (3
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

     (5 (make-random-flet-form size vars))
		
     )))

(defun make-random-integer-case-form (size vars)
  (let ((ncases (1+ (random 10))))
    (if (< (+ size size) (+ ncases 2))
	;; Too small, give up
	(make-random-integer-form size vars)
      (let* ((sizes (random-partition (1- size) (+ ncases 2)))
	     (bound (ash 1 (+ 2 (random 16))))
	     (lower-bound (if (coin 3) 0 (- bound)))
	     (upper-bound (if (and (< lower-bound 0) (coin 3))
			      1
			    (1+ bound)))
	     (cases
	      (loop
	       for case-size in (cddr sizes)
	       for vals = (loop repeat (1+ (min (random 10) (random 10)))
				collect (random-from-interval
					 upper-bound lower-bound))
	       for result = (make-random-integer-form case-size vars)
	       repeat ncases
	       collect `(,vals ,result)))
	     (expr (make-random-integer-form (first sizes) vars)))
	`(case ,expr
	   ,@cases
	   (t ,(make-random-integer-form (second sizes) vars)))))))

(defun make-random-flet-form (size vars)
  "Generate random flet, labels forms, for now with no arguments
   and a single binding per form."
  (let ((fname (random-from-seq #(%f1 %f2 %f3 %f4 %f5 %f6 %f7 %f8 %f9 %f10
				  %f11 %f12 %f13 %f14 %f15 %f16 %f17 %f18))))
    (if (member fname *flet-names* :test #'eq)
	(make-random-integer-form size vars)
      (destructuring-bind (s1 s2) (random-partition (max 2 (1- size)) 2)
	(let ((op (random-from-seq #(flet labels)))
	      (form1
	       ;; Allow return-from of the flet/labels function
	       (let ((*random-int-form-blocks*
		      (cons fname *random-int-form-blocks*)))
		 (make-random-integer-form s1 vars)))
	      (form2 (let ((*flet-names* (cons fname *flet-names*)))
		       (make-random-integer-form s2 vars))))
	  `(,op ((,fname () ,form1)) ,form2))))))

(defun make-random-pred-form (size vars)
  "Make a random form whose value is to be used as a generalized boolean."
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

(defun make-random-element-of-type (type)
  "Create a random element of a lisp type."
  (cond
   ((consp type)
    (let ((type-op (first type)))
      (ecase type-op
	(integer
	 (let ((lo (let ((lo (cadr type)))
		     (cond
		      ((consp lo) (1+ (car lo)))
		      ((eq lo nil) '*)
		      (t lo))))
	       (hi (let ((hi (caddr type)))
		     (cond
		      ((consp hi) (1- (car hi)))
		      ((eq hi nil) '*)
		      (t hi)))))
	 (if (eq lo '*)
	     (if (eq hi '*)
		 (let ((x (ash 1 (random 35))))
		   (random-from-interval x (- x)))
	       (random-from-interval (1+ hi)
				     (- hi (random (ash 1 35)))))

	   (if (eq hi '*)
	       (random-from-interval (+ lo (random (ash 1 35)) 1)
				     lo)
	     ;; May generalize the next case to increase odds
	     ;; of certain integers (near 0, near endpoints, near
	     ;; powers of 2...)
	     (random-from-interval (1+ hi) lo)))))
	(mod
	 (let ((modulus (second type)))
	   (assert (and (integerp modulus)
			(plusp modulus)))
	   (make-random-element-of-type `(integer 0 (,modulus)))))
	(unsigned-byte
	 (if (null (cdr type))
	   (make-random-element-of-type '(integer 0 *))
	   (let ((bits (second type)))
	     (if (eq bits'*)
		 (make-random-element-of-type '(integer 0 *))
	       (progn
		 (assert (and (integerp bits) (>= bits 1)))
		 (make-random-element-of-type `(integer 0 ,(1- (ash 1 bits)))))))))
	)))
   (t
    (ecase type
      (bit (random 2))
      (boolean (random-from-seq #(nil t)))
      (symbol (random-from-seq #(nil t a b c :a :b :c |z| foo |foo| cl:car)))
      (unsigned-byte (random-from-interval (1+ (ash 1 (random 35))) 0))
      (integer (let ((x (ash 1 (random 35))))
		 (random-from-interval (1+ x) (- x))))
      ))))
	
(defun random-partition (n p)
  "Partition n into p numbers, each >= 1.  Return list of numbers."
  (assert (<= 1 p))
  (cond
   ((= p 1) (list n))
   ((< n p) (make-list p :initial-element 1))
   (t
    (let ((n1 (1+ (random (floor n p)))))
      (cons n1 (random-partition (- n n1) (1- p)))))))

(defun make-optimized-lambda-form (form vars var-types)
  `(lambda ,vars
     (declare ,@(mapcar #'(lambda (tp var)
			    `(type ,tp ,var))
			var-types vars)
	      (ignorable ,@vars)
	      (optimize #+cmu (extensions:inhibit-warnings 3)
			(speed 3) (safety 1) (debug 1)))
     ,form))

(defun make-unoptimized-lambda-form (form vars var-types)
  (declare (ignore var-types))
  `(lambda ,vars
     (declare (notinline ,@(fn-symbols-in-form form))
	      (optimize #+cmu (extensions:inhibit-warnings 3)
			(safety 3) (speed 0) (debug 3))
	      (ignorable ,@vars))
     ,form))

(defun test-int-form (form vars var-types vals-list)
  ;; Try to compile FORM with associated VARS, and if it compiles
  ;; check for equality of the two compiled forms.
  ;; Return a non-nil list of details if a problem is found,
  ;; NIL otherwise.
  (let ((optimized-fn-src (make-optimized-lambda-form form vars var-types))
	(unoptimized-fn-src (make-unoptimized-lambda-form form vars var-types)))
    (setq *int-form-vals* nil
	  *optimized-fn-src* optimized-fn-src
	  *unoptimized-fn-src* unoptimized-fn-src)
    (flet ((%compile
	    (lambda-form)
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
					      :lambda-form lambda-form
					      :compiler-condition
					      (with-output-to-string
						(*standard-output*)
						(let ((*print-escape* t))
						  (prin1 c)))))))))
	     (compile nil lambda-form))))
      (let ((optimized-compiled-fn   (%compile optimized-fn-src))
	    (unoptimized-compiled-fn (%compile unoptimized-fn-src)))
	(declare (type function optimized-compiled-fn unoptimized-compiled-fn))
	(dolist (vals vals-list)
	  (setq *int-form-vals* vals)
	  (flet ((%eval-error
		  (kind)
		  (return
		   (list (list :vars vars
			       :vals vals
			       :form form
			       :var-types var-types
			       :optimized-lambda-form optimized-fn-src
			       :unoptimized-lambda-form unoptimized-fn-src
			       :kind kind)))))
	      
	    (let ((unopt-result
		   (handler-case
		    (identity ;; multiple-value-list
		     (apply unoptimized-compiled-fn vals))
		    (error (c) (declare (ignore c))
			   (%eval-error :unoptimized-form-error))))
		  (opt-result
		   (handler-case
		    (identity ;; multiple-value-list
		     (apply optimized-compiled-fn vals))
		    (error (c) (declare (ignore c))
			   (%eval-error :optimized-form-error)))))
	      (if (equal opt-result unopt-result)
		  nil
		(progn
		  (format t "Different results: ~A, ~A~%"
			  opt-result unopt-result)
		  (setq *opt-result* opt-result
			*unopt-result* unopt-result)
		  (%eval-error (list :different-results
				     opt-result
				     unopt-result)))))))))))

;;; Interface to the form pruner

(defun prune-int-form (form vars var-types vals-list)
  "Conduct tests on selected simplified versions of FORM.  Return the
   minimal form that still causes some kind of failure."
  (flet ((%try-fn (new-form)
		  (when (test-int-form new-form vars var-types vals-list)
		    (setf form new-form)
		    (throw 'success nil))))
    (loop
     (catch 'success
       (prune form #'%try-fn)
       (return form)))))

(defun prune-results (result-list)
  "Given a list of test results, prune their forms down to a minimal set."
  (loop for result in result-list
	collect
	(let* ((form (getf result :form))
	       (vars (getf result :vars))
	       (var-types (getf result :var-types))
	       (vals-list (list (getf result :vals)))
	       (pruned-form (prune-int-form form vars var-types vals-list))
	       (optimized-lambda-form (make-optimized-lambda-form
				       pruned-form vars var-types))
	       (unoptimized-lambda-form (make-unoptimized-lambda-form
					 pruned-form vars var-types)))
	    `(:vars ,vars
	      :var-types ,var-types
	      :vals ,(first vals-list)
	      :form ,pruned-form
	      :optimized-lambda-form ,optimized-lambda-form
	      :unoptimized-lambda-form ,unoptimized-lambda-form))))

;;;
;;; The call (PRUNE form try-fn) attempts to simplify the lisp form
;;; so that it still satisfies TRY-FN.  The function TRY-FN should
;;; return if the substitution is a failure.  Otherwise, it should
;;; transfer control elsewhere via GO, THROW, etc.
;;;
;;; The return value of PRUNE should be ignored.
;;;
(defun prune (form try-fn)
  (declare (function try-fn))
  (flet ((try (x) (funcall try-fn x)))
    (when (consp form)
      (let ((op (car form))
	    (args (cdr form)))
	(case op
	 
	 ((signum integer-length logcount
		  gcd lcm logandc1 logandc2 lognand lognor logorc1 logorc2
		  realpart imagpart)
	  (mapc #'try args)
	  (try 0)
	  (try 1)
	  (try -1)
	  (prune-fn form try-fn))
	 
	 ((abs 1+ 1- identity values)
	  (mapc #'try args)
	  (prune-fn form try-fn))
	 
	 ((- + * min max logand logior logxor logeqv
	     and or not eq eql equal = < > <= >= /=)
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
	  (when (and (consp (first args))
		     (eq 'byte (first (first args)))
		     (every #'numberp (cdr (first args)))
		     (numberp (second args)))
	    (try (eval form)))
	  (prune-fn form try-fn))

	 ((ldb-test)
	  (prune-fn form try-fn))

	 ((let let*)
	  (prune-let form try-fn))

	 ((block)
	  (let ((name (second form))
		(body (cddr form)))
	    (when (and body (null (cdr body)))
	      (let ((form1 (first body)))

		;; Try removing the block entirely if it is not in use
		(when (not (find-in-tree name body))
		  (try form1))
		
		;; Try removing the block if its only use is an immediately
		;; enclosed return-from: (block <n> (return-from <n> <e>))
		(when (and (consp form1)
			   (eq (first form1) 'return-from)
			   (eq (second form1) name)
			   (not (find-in-tree name (third form1))))
		  (try (third form1)))
		
		;; Otherwise, try to simplify the subexpression
		(prune form1
		       #'(lambda (x)
			   (try `(block ,name ,x))))))))

	 ((flet labels)
	  (prune-flet form try-fn))

	 ((case)
	  (prune-case form try-fn))

	 ((isqrt)
	  (let ((arg (second form)))
	    (assert (null (cddr form)))
	    (assert (consp arg))
	    (assert (eq (first arg) 'abs))
	    (let ((arg2 (second arg)))
	      (try arg2)
	      ;; Try to fold
	      (when (integerp arg2)
		(try (isqrt (abs arg2))))
	      ;; Otherwise, simplify arg2
	      (prune arg2 #'(lambda (form)
			      (try `(isqrt (abs ,form))))))))

	 ((ash)
	  (let ((form1 (second form))
		(form2 (third form)))
	    (try form1)
	    (try form2)
	    (prune form1
		   #'(lambda (form)
		       (try `(ash ,form ,form2))))
	    (when (and (consp form2)
		       (= (length form2) 3))
	      (when (and (integerp form1)
			 (eq (first form2) 'min)
			 (every #'integerp (cdr form2)))
		(try (eval form)))
	      (let ((form3 (third form2)))
		(prune form3
		       #'(lambda (form)
			   (try
			    `(ash ,form1 (,(first form2) ,(second form2)
					  ,form)))))))))

	 ((floor ceiling truncate round mod rem)
	  (let ((form1 (second form))
		(form2 (third form)))
	    (try form1)
	    (when (cddr form) (try form2))
	    (prune form1
		   (if (cddr form)
		       #'(lambda (form)
			   (try `(,op ,form ,form2)))
		     #'(lambda (form) (try `(,op ,form)))))
	    (when (and (consp form2)
		       (= (length form2) 3))
	      (when (and (integerp form1)
			 (member (first form2) '(max min))
			 (every #'integerp (cdr form2)))
		(try (eval form)))
	      (let ((form3 (third form2)))
		(prune form3
		       #'(lambda (form)
			   (try
			    `(,op ,form1 (,(first form2) ,(second form2)
					  ,form)))))))))
	 
	 (otherwise
	  (prune-fn form try-fn))
	 )))))

(defun find-in-tree (value tree)
  "Return true if VALUE is eql to a node in TREE."
  (or (eql value tree)
      (and (consp tree)
	   (or (find-in-tree value (car tree))
	       (find-in-tree value (cdr tree))))))

(defun prune-list (list element-prune-fn list-try-fn)
  (declare (type function element-prune-fn list-try-fn))
  "Utility function for pruning in a list."
    (loop for i from 0
	  for e in list
	  do (funcall element-prune-fn
		      e
		      #'(lambda (form)
			  (funcall list-try-fn
				   (append (subseq list 0 i)
					   (list form)
					   (subseq list (1+ i))))))))

(defun prune-case (form try-fn)
  (declare (type function try-fn))
  (flet ((try (e) (funcall try-fn e)))
    (let* ((op (first form))
	   (expr (second form))
	   (cases (cddr form)))
      
      ;; Try just the top expression
      (try expr)
      
      ;; Try simplifying the expr
      (prune expr
	     #'(lambda (form)
		 (try `(,op ,form ,@cases))))
      
      ;; Try individual cases
      (loop for case in cases
	    do (try (first (last (rest case)))))
      
      ;; Try deleting individual cases
      (loop for i from 0 below (1- (length cases))
	    do (try `(,op ,expr
			  ,@(subseq cases 0 i)
			  ,@(subseq cases (1+ i)))))
      
      ;; Try simplifying the cases
      ;; Assume each case has a single form
      (prune-list cases
		  #'(lambda (case try-fn)
		      (declare (function try-fn))
		      (when (eql (length case) 2)
			(prune (cadr case)
			       #'(lambda (form)
				   (funcall try-fn
					    (list (car case) form))))))
		  #'(lambda (cases)
		      (try `(,op ,expr ,@cases)))))))

(defun prune-fn (form try-fn)
  "Attempt to simplify a function call form.  It is considered
   acceptable to replace the call by one of its argument forms."
  (declare (function try-fn))
  (prune-list (cdr form)
	      #'prune
	      #'(lambda (args)
		  (funcall try-fn (cons (car form) args)))))

(defun prune-let (form try-fn)
  "Attempt to simplify a LET form."
  (declare (function try-fn))
  (let* ((op (car form))
	 (binding-list (cadr form))
	 (body (cddr form))
	 (body-len (length body))
	 (len (length binding-list)))

    ;; Try to simplify (let ((<name> <form>)) <name>) to <form>
    (when (and (eql len 1)
	       (eql body-len 1)
	       (eql (caar binding-list) (car body)))
      (funcall try-fn (cadar binding-list)))

    ;; Try to simplify the forms in the RHS of the bindings
    (prune-list binding-list
		#'(lambda (binding try-fn)
		    (declare (function try-fn))
		    (prune (cadr binding)
			   #'(lambda (form)
			       (funcall try-fn
					(list (car binding)
					      form)))))
		#'(lambda (bindings)
		    (funcall try-fn `(,op ,bindings ,@body))))

    ;; Try to simplify the body of the LET form
    (when body
      (unless binding-list
	(funcall try-fn (car (last body))))
      (when (and (first binding-list)
		 (not (rest binding-list))
		 (not (rest body)))
	(let ((binding (first binding-list)))
	  (unless (consp (second binding))
	    (funcall try-fn `(let ()
			       ,@(subst (second binding)
					(first binding) body))))))
      (prune (car (last body))
	     #'(lambda (form2)
		 (funcall try-fn
			  `(,@(butlast form) ,form2)))))))

(defun prune-flet (form try-fn)
  "Attempt to simplify a FLET form."
  (declare (function try-fn))

  (let* ((op (car form))
	 (binding-list (cadr form))
	 (body (cddr form)))
    
    ;; Try to simplify the forms in the RHS of the bindings
    (prune-list binding-list
		#'(lambda (binding try-fn)
		    (declare (function try-fn))
		    (prune (third binding)
			   #'(lambda (form)
			       (funcall try-fn
					(list (first binding)
					      (second binding)
					      form)))))
		#'(lambda (bindings)
		    (funcall try-fn `(,op ,bindings ,@body))))

    ;; ;; Try to simplify the body of the FLET form
    (when body

      ;; No bindings -- try to simplify to the last form in the body
      (unless binding-list
	(funcall try-fn (first (last body))))

      (when (and (consp binding-list)
		 (null (rest binding-list)))
	(let ((binding (first binding-list)))
	  ;; One binding -- match on (flet ((<name> () <body>)) (<name>))
	  (when (and (symbolp (first binding))
		     (not (find-in-tree (first binding) (rest binding)))
		     (null (second binding))
		     (equal body (list (list (first binding)))))
	    (funcall try-fn `(,op () ,@(cddr binding))))
	  ;; One binding -- try to remove it if not used
	  (when (and (symbolp (first binding))
		     (not (find-in-tree (first binding) body)))
	    (funcall try-fn (first (last body))))
	))


      ;; Try to simplify (the last form in) the body.
      (prune (first (last body))
	     #'(lambda (form2)
		 (funcall try-fn
			  `(,@(butlast form) ,form2)))))))
