;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct 18 07:23:48 2002
;;;; Contains: Tests for AND

(in-package :cl-test)

(deftest and.1
  (and)
  t)

(deftest and.2
  (and nil)
  nil)

(deftest and.3
  (and 'a)
  a)

(deftest and.4
  (and (values 'a 'b 'c))
  a b c)

(deftest and.5 (and (values)))

(deftest and.6
  (and (values t nil) 'a)
  a)

(deftest and.7
  (and nil (values 'a 'b 'c))
  nil)

(deftest and.8
  (and (values 1 nil) (values nil 2))
  nil 2)

(deftest and.9
  (and (values nil t) t)
  nil)

(deftest and.order.1
  (let ((x 0))
    (values (and nil (incf x))
	    x))
  nil 0)

(deftest and.order.2
  (let ((i 0) a b c d)
    (values
     (and (setf a (incf i))
	  (setf b (incf i))
	  (setf c (incf i))
	  (setf d (incf i)))
     i a b c d))
  4 4 1 2 3 4)

