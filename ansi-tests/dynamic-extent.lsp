;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 21 09:10:52 2005
;;;; Contains: Tests of DYNAMIC-EXTENT

(in-package :cl-test)

(deftest dynamic-extent.1
  (let () (declare (dynamic-extent)))
  nil)

(deftest dynamic-extent.2
  (let ((x 'a))
    (declare (dynamic-extent x))
    x)
  a)

(deftest dynamic-extent.3
  (let ((x (list 'a 'b 'c)))
    (declare (dynamic-extent x))
    (length x))
  3)

(deftest dynamic-extent.4
  (let ((x (vector 'a 'b 'c)))
    (declare (dynamic-extent x))
    (length x))
  3)

(deftest dynamic-extent.5
  (flet ((%f (x) (list 'a x)))
    (declare (dynamic-extent (function %f)))
    (mapcar #'%f '(1 2 3)))
  ((a 1) (a 2) (a 3)))

(deftest dynamic-extent.6
  (labels ((%f (x) (list 'a x)))
    (declare (dynamic-extent (function %f)))
    (mapcar #'%f '(1 2 3)))
  ((a 1) (a 2) (a 3)))

(deftest dynamic-extent.7
  (labels ((%f (x) (if (consp x)
		       (cons (%f (car x)) (%f (cdr x)))
		     '*)))
    (declare (dynamic-extent (function %f)))
    (mapcar #'%f '((1) 2 (3 4 5))))
  ((* . *) * (* * * . *)))

(deftest dynamic-extent.8
  (let ((x (+ most-positive-fixnum 2)))
    (declare (dynamic-extent x))
    (1- x))
  #.(1+ most-positive-fixnum))

(deftest dynamic-extent.9
  (flet ((f () (list 'a 'b)))
    (let ((f (list 'c 'd)))
      (declare (dynamic-extent (function f)))
      f))
  (c d))

(deftest dynamic-extent.10
  (let ((x nil))
    (values
     x
     (locally (declare (dynamic-extent x) (notinline length))
	      (setq x (list 'a 'b 'c 'd 'e))
	      (prog1 (length x) (setq x t)))
     x))
  nil 5 t)

(deftest dynamic-extent.11
  (let* ((x (list 'a 'b))
	 (y (cons 'c x)))
    (declare (dynamic-extent y))
    (cdr y))
  (a b))











