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
  (signals-error (vector-pop (vector 1 2 3)) type-error)
  t)

(deftest vector-pop.error.2
  (let ((v (make-array '(5) :initial-element 'x
		       :fill-pointer 0)))
    (handler-case (vector-pop v)
		  (error () 'error)))
  error)

(deftest vector-pop.error.3
  (signals-error (vector-pop) program-error)
  t)

(deftest vector-pop.error.4
  (signals-error (let ((v (make-array '(5) :fill-pointer t
				       :initial-element 'x)))
		    (vector-pop v nil))
		 program-error)
  t)

(deftest vector-pop.error.5
  (signals-error (locally (vector-pop (vector 1 2 3)) t) type-error)
  t)
