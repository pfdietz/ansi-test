;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan 21 22:14:23 2003
;;;; Contains: Tests of FILL-POINTER

(in-package :cl-test)

;;; More tests are in make-array.lsp

(deftest fill-pointer.1
  (fill-pointer (make-array '(10) :fill-pointer 5))
  5)

(deftest fill-pointer.2
  (fill-pointer (make-array '(10) :fill-pointer t))
  10)

(deftest fill-pointer.3
  (let ((a (make-array '(10) :fill-pointer 5
		       :initial-contents '(1 2 3 4 5 6 7 8 9 10))))
    (values
     (fill-pointer a)
     (setf (fill-pointer a) 6)
     a))
  5 6 #(1 2 3 4 5 6))





