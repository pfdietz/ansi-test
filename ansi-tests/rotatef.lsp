;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 15:44:38 2003
;;;; Contains: Tests for ROTATEF

(in-package :cl-test)

(deftest rotatef-order.1
  (let ((x (vector 'a 'b 'c 'd 'e 'f))
	(i 2))
    (values
     (rotatef (aref x (incf i)) (aref x (incf i)))
     x i))
  nil
  #(a b c e d f)
  4)

(deftest rotatef-order.2
  (let ((x (vector 'a 'b 'c 'd 'e 'f))
	(i 2))
    (values
     (rotatef (aref x (incf i)) (aref x (incf i)) (aref x (incf i)))
     x i))
  nil
  #(a b c e f d)
  5)

(deftest rotatef.1
  (let ((x (vector 0 1 2)))
    (values
     (rotatef (aref x (aref x 0)) (aref x (aref x 1)) (aref x (aref x 2)))
     x))
  nil
  #(1 2 0))

(deftest rotatef.2
  (let ((x (vector 0 1 2 3 4 5 6 7 8 9)))
    (values
     (rotatef (aref x (aref x 0))
	      (aref x (aref x 1))
	      (aref x (aref x 2))
	      (aref x (aref x 3))
	      (aref x (aref x 4))
	      (aref x (aref x 5))
	      (aref x (aref x 6))
	      (aref x (aref x 7))
	      (aref x (aref x 8))
	      (aref x (aref x 9)))
     x))
  nil
  #(1 2 3 4 5 6 7 8 9 0))

(deftest rotatef.3
  (rotatef)
  nil)

(deftest rotatef.4
  (let ((x 10))
    (values
     x
     (rotatef x)
     x))
  10 nil 10)

(deftest rotatef.5
  (let ((x 'a) (y 'b))
    (values x y (rotatef x y) x y))
  a b nil b a)
  

;;; Need to add more tests for ROTATEF here
