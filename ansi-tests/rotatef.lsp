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

;;; Need to add more tests for ROTATEF here
