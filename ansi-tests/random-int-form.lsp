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

(declaim (special *optimized-fn-src* *unoptimized-fn-src* *int-form-vals*))

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
				`(integer ,(car range) ,(cadr range)))
			    var-ranges))
	 (optimized-fn-src
	  `(lambda ,vars (declare ,@(mapcar #'(lambda (tp var)
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
    (setq *optimized-fn-src* optimized-fn-src
	  *unoptimized-fn-src* unoptimized-fn-src)
    (#+sbcl handler-bind
     #+sbcl ((sb-ext::compiler-note #'muffle-warning))
     #-sbcl progn
     (let ((optimized-compiled-fn (compile nil optimized-fn-src))
	   (unoptimized-compiled-fn (compile nil unoptimized-fn-src)))
       (dotimes (i 20 nil)
	 (let ((vals
		(mapcar #'(lambda (range)
			    (let ((lo (car range))
				  (hi (cadr range)))
			      (random-from-interval lo (1+ hi))))
			var-ranges)))
	   (setq *int-form-vals* vals)
	   (let ((opt-result (apply unoptimized-compiled-fn vals))
		 (unopt-result (apply optimized-compiled-fn vals)))
	     (if (equal opt-result unopt-result)
		 nil
	       (return (list (list vars vals
				   unoptimized-fn-src
				   optimized-fn-src
				   )))))))))))

(defun fn-symbols-in-form (form)
  "Return a list of the distinct standardized lisp function
   symbols occuring ing FORM.  These are used to generate a NOTINLINE
   declaration for the unoptimized form."
  (intersection
   (remove-duplicates (fn-symbols-in-form* form) :test #'eq)
   *cl-function-symbols*))

(defun fn-symbols-in-form* (form)
  (when (consp form)
    (if (symbolp (car form))
	(cons (car form) (mapcan #'fn-symbols-in-form* (cdr form)))
      (mapcan #'fn-symbols-in-form* form))))

(defun make-random-integer-range (var)
  "Generate a list (LO HI) of integers, LO <= HI.  This is used
   for generating integer types."
  (declare (ignore var))
  (flet ((%r () (let ((r (ash 1 (1+ (random 32)))))
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
    (ecase (random 7)
     ;; Unary ops
     ((0 1)
      (let ((op (random-from-seq '(- abs signum 1+ 1- identity
				     integer-length logcount))))
	`(,op ,(make-random-integer-form (1- size) vars))))
     ;; Binary op
     ((2 3 4 5)
      (let* ((op (random-from-seq
		  '(+ - * logand min max ;; gcd lcm
		      logandc1 logandc2 logeqv logior lognand lognor logorc1
		      logorc2 logxor)))
	     (leftsize (random size)))
	`(,op ,(make-random-integer-form leftsize vars)
	      ,(make-random-integer-form (- size 1 leftsize) vars))))
     ;; conditionals
     (6
      (let* ((cond-size (random (max 1 (floor size 2))))
	     (then-size (random (- size cond-size)))
	     (else-size (- size 1 cond-size then-size))
	     (pred (make-random-pred-form cond-size vars))
	     (then-part (make-random-integer-form then-size vars))
	     (else-part (make-random-integer-form else-size vars)))
	`(if ,pred ,then-part ,else-part)))
     )))

(defun make-random-pred-form (size vars)
  (if (<= size 1)
      (ecase (random 3)
	(0 (if (coin) t nil))
	((1 2)
	 `(,(random-from-seq '(< <= = > >= /=))
	   ,(make-random-integer-form size vars)
	   ,(make-random-integer-form size vars))))
    (ecase (random 5)
      (0 (if (coin) t nil))
      (1 `(not ,(make-random-pred-form (1- size) vars)))
      (2 (let* ((leftsize (random size))
		(rightsize (- size 1 leftsize)))
	   `(,(random-from-seq '(and or))
	     ,(make-random-pred-form leftsize vars)
	     ,(make-random-pred-form rightsize vars))))
      (3 (let* ((leftsize (random size))
		(rightsize (- size 1 leftsize)))
	   `(,(random-from-seq '(< <= > >= = /=))
	     ,(make-random-integer-form leftsize vars)
	     ,(make-random-integer-form rightsize vars))))
      (4 (let* ((cond-size (random (max 1 (floor size 2))))
		(then-size (random (- size cond-size)))
		(else-size (- size 1 cond-size then-size))
		(pred (make-random-pred-form cond-size vars))
		(then-part (make-random-pred-form then-size vars))
		(else-part (make-random-pred-form else-size vars)))
	   `(if ,pred ,then-part ,else-part)))
      )))

    
	
	    
