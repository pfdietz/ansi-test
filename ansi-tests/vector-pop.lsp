;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Jan 24 07:46:29 2003
;;;; Contains: Tests for VECTOR-POP

(in-package :cl-test)

(deftest vector-pop.1
  (let ((v (make-array '(5) :initial-contents '(a b c d e)
		       :fill-pointer 3)))
    (values
     (length v)
     (check-values (vector-pop v))
     (fill-pointer v)
     (length v)
     v))
  3 c 2 2 #(a b))

;;; Error cases

(deftest vector-pop.error.1
  (classify-error (vector-pop (vector 1 2 3)))
  type-error)

(deftest vector-pop.error.2
  (let ((v (make-array '(5) :initial-element 'x
		       :fill-pointer 0)))
    (handler-case (vector-pop v)
		  (error () 'error)))
  error)

(deftest vector-pop.error.3
  (classify-error (vector-pop))
  program-error)

(deftest vector-pop.error.4
  (classify-error (let ((v (make-array '(5) :fill-pointer t
				       :initial-element 'x)))
		    (vector-pop v nil)))
  program-error)

(deftest vector-pop.error.5
  (classify-error (locally (vector-pop (vector 1 2 3)) t))
  type-error)
