;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct 18 07:29:27 2002
;;;; Contains: Tests of OR

(in-package :cl-test)

(deftest or.1
  (or)
  nil)

(deftest or.2
  (or nil)
  nil)

(deftest or.3
  (or 'a)
  a)

(deftest or.4
  (or (values 'a 'b 'c))
  a b c)

(deftest or.5 (or (values)))

(deftest or.6
  (or (values t nil) 'a)
  t)

(deftest or.7
  (or nil (values 'a 'b 'c))
  a b c)

(deftest or.8
  (let ((x 0))
    (values (or t (incf x))
	    x))
  t 0)

(deftest or.9
  (or (values nil 1 2) (values 1 nil 2))
  1 nil 2)




  

