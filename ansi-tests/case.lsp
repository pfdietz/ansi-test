;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct 18 19:56:44 2002
;;;; Contains: Tests of CASE

(in-package :cl-test)

(deftest case.1
  (case 'a)
  nil)

(deftest case.2
  (case 10 (10 'a))
  a)

(deftest case.3
  (case (copy-seq "abc") ("abc" 'a))
  nil)

(deftest case.4
  (case 'z ((a b c) 1)
	   ((d e) 2)
	   ((f z g) 3)
	   (t 4))
  3)

(deftest case.5
  (case (1+ most-positive-fixnum)
    (#.(1+ most-positive-fixnum) 'a))
  a)

(deftest case.6
  (case nil (nil 'a) (t 'b))
  b)

(deftest case.7
  (case nil ((nil) 'a) (t 'b))
  a)

(deftest case.8
  (case 'a (b 0) (a (values 1 2 3)) (t nil))
  1 2 3)

(deftest case.9
  (case 'c (b 0) (a (values 1 2 3)) (t (values 'x 'y 'z)))
  x y z)

(deftest case.10
  (case 'z (b 1) (a 2) (z (values)) (t nil)))

(deftest case.11
  (case 'z (b 1) (a 2) (t (values))))

(deftest case.12
  (case t (a 10))
  nil)

(deftest case.13
  (case t ((t) 10) (t 20))
  10)

(deftest case.14
  (let ((x (list 'a 'b)))
    (eval `(case (quote ,x) ((,x) 1) (t 2))))
  1)

(deftest case.15
  (case 'otherwise ((t) 10))
  nil)

(deftest case.16
  (case t ((otherwise) 10))
  nil)

(deftest case.17
  (case 'a (b 0) (c 1) (otherwise 2))
  2)

(deftest case.18
  (case 'a (b 0) (c 1) ((otherwise) 2))
  nil)

(deftest case.19
  (case 'a (b 0) (c 1) ((t) 2))
  nil)

(deftest case.20
  (case #\a
    ((#\b #\c) 10)
    ((#\d #\e #\A) 20)
    (() 30)
    ((#\z #\a #\y) 40))
  40)




