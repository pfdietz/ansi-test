;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct 18 23:16:23 2002
;;;; Contains: Tests for MULTIPLE-VALUE-BIND

(in-package :cl-test)

(deftest multiple-value-bind.1
  (multiple-value-bind (x y z) (values 1 2 3)
    (declare (type integer x))
    (declare (type integer y))
    (declare (type integer z))
    (list z y x))
  (3 2 1))

(deftest multiple-value-bind.2
  (multiple-value-bind (x y z) (values 1 2 3)
    (let ((x 4))
      (list x y z)))
  (4 2 3))

(deftest multiple-value-bind.3
  (multiple-value-bind (x y z) (values 1 2 3 4 5 6)
    (list x y z))
  (1 2 3))

(deftest multiple-value-bind.4
  (multiple-value-bind (x y z) (values 1 2)
    (list x y z))
  (1 2 nil))

(deftest multiple-value-bind.5
  (multiple-value-bind () (values 1 2) (values 'a 'b 'c))
  a b c)

(deftest multiple-value-bind.6
  (multiple-value-bind (x y z) (values)
    (list x y z))
  (nil nil nil))

(deftest multiple-value-bind.7
  (let ((z 0) x y)
    (declare (special z))
    (values
     (flet ((%x () (symbol-value 'x))
	    (%y () (symbol-value 'y))
	    (%z () (symbol-value 'z)))
       (multiple-value-bind (x y z) (values 1 2 3)
	 (declare (special x y))
	 (list (%x) (%y) (%z))))
     x y z))
  (1 2 0) nil nil 0)

;;; (deftest multiple-value-bind.error.1
;;;  (classify-error (multiple-value-bind))
;;;  program-error)
;;;
;;; (deftest multiple-value-bind.error.2
;;;  (classify-error (multiple-value-bind (a b c)))
;;;  program-error)

  