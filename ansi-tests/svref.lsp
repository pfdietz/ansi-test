;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Jan 22 21:39:30 2003
;;;; Contains: Tests of SVREF

(in-package :cl-test)

(deftest svref.1
  (let ((a (vector 1 2 3 4)))
    (loop for i below 4 collect (svref a i)))
  (1 2 3 4))

(deftest svref.2
  (let ((a (vector 1 2 3 4)))
    (values
     (loop for i below 4
	   collect (setf (svref a i) (+ i 10)))
     a))
  (10 11 12 13)
  #(10 11 12 13))

;;; Error tests

(deftest svref.error.1
  (classify-error (svref))
  program-error)

(deftest svref.error.2
  (classify-error (svref (vector 1)))
  program-error)
