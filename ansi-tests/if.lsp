;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct 18 08:21:29 2002
;;;; Contains: Tests for IF

(in-package :cl-test)

(deftest if.1
  (if t 1 2)
  1)

(deftest if.2
  (if nil 1 2)
  2)

(deftest if.3 (if t (values) 'a))

(deftest if.4
  (if nil 'a)
  nil)

(deftest if.5
  (if t (values 'a 'b 'c) 'd)
  a b c)

(deftest if.6
  (if nil 'a (values 'b 'c 'd))
  b c d)

(deftest if.7 (if nil 'a (values)))

(deftest if.order.1
  (let ((i 0))
    (values (if (= (incf i) 1) 't nil) i))
  t 1)
