;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Oct  8 22:55:02 2002
;;;; Contains: Tests of FLET

(in-package :cl-test)

(deftest flet.1
  (flet ((%f () 1))
    (%f))
  1)

(deftest flet.2
  (flet ((%f (x) x))
    (%f 2))
  2)

(deftest flet.3
  (flet ((%f (&rest args) args))
    (%f 'a 'b 'c))
  (a b c))

;;; The optional arguments are not in the block defined by
;;; the local function declaration
(deftest flet.4
  (block %f
    (flet ((%f (&optional (x (return-from %f 10)))
	       20))
      (%f)))
  10)

(deftest flet.5
  (flet ((%f () (return-from %f 15) 35))
    (%f))
  15)

;;; The aux parameters are not in the block defined by
;;; the local function declaration
(deftest flet.6
  (block %f
    (flet ((%f (&aux (x (return-from %f 10)))
	       20))
      (%f)))
  10)

;;; The function is not visible inside itself
(deftest flet.7
  (flet ((%f (x) (+ x 5)))
    (flet ((%f (y) (cond ((eql y 20) 30)
			 (t (%f 20)))))
      (%f 15)))
  25)

;;; Keyword arguments
(deftest flet.8
  (flet ((%f (&key a (b 0 b-p)) (values a b (not (not b-p)))))
    (%f))
  nil 0 nil)

(deftest flet.9
  (flet ((%f (&key a (b 0 b-p)) (values a b (not (not b-p)))))
    (%f :a 1))
  1 0 nil)

(deftest flet.10
  (flet ((%f (&key a (b 0 b-p)) (values a b (not (not b-p)))))
    (%f :b 2))
  nil 2 t)

(deftest flet.11
  (flet ((%f (&key a (b 0 b-p)) (values a b (not (not b-p)))))
    (%f :b 2 :a 3))
  3 2 t)

;;; Unknown keyword parameter should throw a program-error in safe code
;;; (section 3.5.1.4)
(deftest flet.12
  (classify-error
   (flet ((%f (&key a (b 0 b-p)) (values a b (not (not b-p))))) (%f :c 4)))
  program-error)

;;; Odd # of keyword args should throw a program-error in safe code
;;; (section 3.5.1.6)
(deftest flet.13
  (classify-error
   (flet ((%f (&key a (b 0 b-p)) (values a b (not (not b-p))))) (%f :a)))
  program-error)

;;; Too few arguments (section 3.5.1.2)
(deftest flet.14
  (classify-error (flet ((%f (a) a)) (%f)))
  program-error)

;;; Too many arguments (section 3.5.1.3)
(deftest flet.15
  (classify-error (flet ((%f (a) a)) (%f 1 2)))
  program-error)

;;; Invalid keyword argument (section 3.5.1.5)
(deftest flet.16
  (classify-error (flet ((%f (&key a) a)) (%f '(foo))))
  program-error)


;;; Definition of a (setf ...) function

(deftest flet.17
  (flet (((setf %f) (x y) (setf (car x) y)))
    (let ((z (list 1 2)))
      (setf (%f z) 'a)
      z))
  (a 2))

;;; Body is an implicit progn
(deftest flet.18
  (flet ((%f (x) (incf x) (+ x x)))
    (%f 10))
  22)

;;; Can handle at least 50 lambda parameters
(deftest flet.19
  (flet ((%f (a1 a2 a3 a4 a5 a6 a7 a8 a9 a10
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

;;; flet works with a large (maximal?) number of arguments
(deftest flet.20
  (let* ((n (min lambda-parameters-limit 1024))
	 (vars (loop for i from 1 to n collect (gensym))))
    (eval
     `(eql ,n
	   (flet ((%f ,vars (+ ,@ vars)))
	     (%f ,@(loop for e in vars collect 1))))))
  t)

;;; Declarations and documentation strings are ok
(deftest flet.21
  (flet ((%f (x)
	     (declare (type fixnum x))
	     "Add one to the fixnum x."
	     (1+ x)))
    (declare (ftype (fixnum) integer))
    (%f 10))
  11)
