;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Oct  9 19:06:33 2002
;;;; Contains: Tests of LABELS

(in-package :cl-test)

(deftest labels.1
  (labels ((%f () 1))
    (%f))
  1)

(deftest labels.2
  (labels ((%f (x) x))
    (%f 2))
  2)

(deftest labels.3
  (labels ((%f (&rest args) args))
    (%f 'a 'b 'c))
  (a b c))

;;; The optional arguments are not in the block defined by
;;; the local function declaration
(deftest labels.4
  (block %f
    (labels ((%f (&optional (x (return-from %f 10)))
	       20))
      (%f)))
  10)

(deftest labels.5
  (labels ((%f () (return-from %f 15) 35))
    (%f))
  15)

;;; The aux parameters are not in the block defined by
;;; the local function declaration
(deftest labels.6
  (block %f
    (labels ((%f (&aux (x (return-from %f 10)))
	       20))
      (%f)))
  10)

;;; The function is visible inside itself
(deftest labels.7
  (labels ((%f (x n) (cond ((eql n 0) x)
			   (t (%f (+ x n) (1- n))))))
    (%f 0 10))
  55)

;;; Keyword arguments
(deftest labels.8
  (labels ((%f (&key a (b 0 b-p)) (values a b (not (not b-p)))))
    (%f))
  nil 0 nil)

(deftest labels.9
  (labels ((%f (&key a (b 0 b-p)) (values a b (not (not b-p)))))
    (%f :a 1))
  1 0 nil)

(deftest labels.10
  (labels ((%f (&key a (b 0 b-p)) (values a b (not (not b-p)))))
    (%f :b 2))
  nil 2 t)

(deftest labels.11
  (labels ((%f (&key a (b 0 b-p)) (values a b (not (not b-p)))))
    (%f :b 2 :a 3))
  3 2 t)

;;; Unknown keyword parameter should throw a program-error in safe code
;;; (section 3.5.1.4)
(deftest labels.12
  (classify-error
   (labels ((%f (&key a (b 0 b-p)) (values a b (not (not b-p))))) (%f :c 4)))
  program-error)

;;; Odd # of keyword args should throw a program-error in safe code
;;; (section 3.5.1.6)
(deftest labels.13
  (classify-error
   (labels ((%f (&key a (b 0 b-p)) (values a b (not (not b-p))))) (%f :a)))
  program-error)

;;; Too few arguments (section 3.5.1.2)
(deftest labels.14
  (classify-error (labels ((%f (a) a)) (%f)))
  program-error)

;;; Too many arguments (section 3.5.1.3)
(deftest labels.15
  (classify-error (labels ((%f (a) a)) (%f 1 2)))
  program-error)

;;; Invalid keyword argument (section 3.5.1.5)
(deftest labels.16
  (classify-error (labels ((%f (&key a) a)) (%f '(foo))))
  program-error)


;;; Definition of a (setf ...) function

(deftest labels.17
  (labels (((setf %f) (x y) (setf (car y) x)))
    (let ((z (list 1 2)))
      (setf (%f z) 'a)
      z))
  (a 2))

;;; Scope of defined function names includes &AUX parameters

(deftest labels.7b
  (labels ((%f (x &aux (b (%g x))) b)
	   (%g (y) (+ y y)))
    (%f 10))
  20)

;;; Body is an implicit progn
(deftest labels.18
  (labels ((%f (x) (incf x) (+ x x)))
    (%f 10))
  22)

;;; Can handle at least 50 lambda parameters
(deftest labels.19
  (labels ((%f (a1 a2 a3 a4 a5 a6 a7 a8 a9 a10
	        b1 b2 b3 b4 b5 b6 b7 b8 b9 b10
		c1 c2 c3 c4 c5 c6 c7 c8 c9 c10
		d1 d2 d3 d4 d5 d6 d7 d8 d9 d10
		e1 e2 e3 e4 e5 e6 e7 e8 e9 e10)
	       (+ a1 a2 a3 a4 a5 a6 a7 a8 a9 a10
		  b1 b2 b3 b4 b5 b6 b7 b8 b9 b10
		  c1 c2 c3 c4 c5 c6 c7 c8 c9 c10
		  d1 d2 d3 d4 d5 d6 d7 d8 d9 d10
		  e1 e2 e3 e4 e5 e6 e7 e8 e9 e10)))
    (%f 1 2 3 4 5 6 7 8 9 10
	11 12 13 14 15 16 17 18 19 20
	21 22 23 24 25 26 27 28 29 30
	31 32 33 34 35 36 37 38 39 40
	41 42 43 44 45 46 47 48 49 50))
  1275)

;;; labels works with the maximum number of arguments (if
;;; not too many.)
(deftest labels.20
  (let* ((n (min lambda-parameters-limit 1024))
	 (vars (loop for i from 1 to n collect (gensym))))
    (eval
     `(eql ,n
	   (labels ((%f ,vars (+ ,@ vars)))
	     (%f ,@(loop for e in vars collect 1))))))
  t)

;;; Declarations and documentation strings are ok
(deftest labels.21
  (labels ((%f (x)
	     (declare (type fixnum x))
	     "Add one to the fixnum x."
	     (1+ x)))
    (declare (ftype (function (fixnum) integer) %f))
    (%f 10))
  11)

;;; Keywords can be function names
(deftest labels.22
  (labels ((:foo () 10)
	   (:bar () (1+ (:foo))))
    (:bar))
  11)

(deftest labels.23
  (labels ((:foo () 10)
	   (:bar () (1+ (funcall #':foo))))
    (funcall #':bar))
  11)

(deftest labels.24
  (loop for s in *cl-non-function-macro-special-operator-symbols*
	for form = `(classify-error (labels ((,s (x) (foo (1- x)))
					     (foo (y)
						  (if (<= y 0) 'a
						    (,s (1- y)))))
				      (,s 10)))
	unless (eq (eval form) 'a)
	collect s)
  nil)

(deftest labels.25
  (loop for s in *cl-non-function-macro-special-operator-symbols*
	for form = `(classify-error
		     (labels ((,s (x) (foo (1- x)))
			      (foo (y)
				   (if (<= y 0) 'a
				     (,s (1- y)))))
		       (declare (ftype (function (integer) symbol)
				       foo ,s))
		       (,s 10)))
	unless (eq (eval form) 'a)
	collect s)
  nil)

(deftest labels.26
  (loop for s in *cl-non-function-macro-special-operator-symbols*
	for form = `(classify-error
		     (labels (((setf ,s) (&rest args)
			       (declare (ignore args))
			       'a))
		       (setf (,s) 10)))
	unless (eq (eval form) 'a)
	collect s)
  nil)
