;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Aug 20 22:05:20 2003
;;;; Contains: Tests of the EPSILON constants

(in-package :cl-test)

(deftest epsilons.1
  (loop for e in (list short-float-epsilon single-float-epsilon
		       double-float-epsilon long-float-epsilon)
	when (= (float 1 e) (+ (float 1 e) e))
	collect e)
  nil)

(deftest epsilons.2
  (loop for e in (list short-float-negative-epsilon
		       single-float-negative-epsilon
		       double-float-negative-epsilon
		       long-float-negative-epsilon)
	when (= (float 1 e) (- (float 1 e) e))
	collect e)
  nil)

(deftest epsilons.3
  (loop for e in (list short-float-epsilon single-float-epsilon
		       double-float-epsilon long-float-epsilon)
	unless (= (float 1 e) (+ (float 1 e) (/ e 2)))
	collect e)
  nil)

(deftest epsilons.4
  (loop for e in (list short-float-negative-epsilon
		       single-float-negative-epsilon
		       double-float-negative-epsilon
		       long-float-negative-epsilon)
	unless (= (float 1 e) (- (float 1 e) (/ e 2)))
	collect e)
  nil)