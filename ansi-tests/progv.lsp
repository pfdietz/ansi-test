;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Oct 12 10:00:50 2002
;;;; Contains: Tests for PROGV

(in-package :cl-test)

(deftest progv.1
  (progv () () t)
  t)

(deftest progv.2
  (progv '(x) '(1) (not (not (boundp 'x))))
  t)

(deftest progv.3
  (progv '(x) '(1) (symbol-value 'x))
  1)

(deftest progv.4
  (progv '(x) '(1)
    (locally (declare (special x))
	     x))
  1)

(deftest progv.5
  (let ((x 0))
    (progv '(x) '(1) x))
  0)

(deftest progv.6
  (let ((x 0))
    (declare (special x))
    (progv '(x) ()
      (boundp 'x)))
  nil)

(deftest progv.6a
  (let ((x 0))
    (declare (special x))
    (progv '(x) () (setq x 1))
    x)
  0)

(deftest progv.7
  (progv '(x y z) '(1 2 3)
    (locally (declare (special x y z))
	     (values x y z)))
  1 2 3)

(deftest progv.8
  (progv '(x y z) '(1 2 3 4 5 6 7 8)
    (locally (declare (special x y z))
	     (values x y z)))
  1 2 3)

(deftest progv.9
  (let ((x 0))
    (declare (special x))
    (progv '(x y z w) '(1)
      (values (not (not (boundp 'x)))
	      (boundp 'y)
	      (boundp 'z)
	      (boundp 'w))))
  t nil nil nil)

;; forms are evaluated in order

(deftest progv.10
  (let ((x 0) (y 0) (c 0))
    (progv
	(progn (setf x (incf c)) nil)
	(progn (setf y (incf c)) nil)
      (values x y c)))
  1 2 2)
