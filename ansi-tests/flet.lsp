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


    







