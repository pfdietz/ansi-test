;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Aug  4 22:16:00 2003
;;;; Contains: Tests of FLOOR

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")
(compile-and-load "floor-aux.lsp")

(deftest floor.error.1
  (classify-error (floor))
  program-error)

(deftest floor.error.2
  (classify-error (floor 1.0 1 nil))
  program-error)

;;;

(deftest floor.1
  (floor.1-fn)
  nil)

(deftest floor.2
  (floor.2-fn)
  nil)

(deftest floor.3
  (floor.3-fn 2.0s4)
  nil)

(deftest floor.4
  (floor.3-fn 2.0f4)
  nil)

(deftest floor.5
  (floor.3-fn 2.0d4)
  nil)

(deftest floor.6
  (floor.3-fn 2.0l4)
  nil)

(deftest floor.7
  (floor.7-fn)
  nil)

(deftest floor.8
  (floor.8-fn)
  nil)

(deftest floor.9
  (floor.9-fn)
  nil)

(deftest floor.10
  (loop for x in (remove-if #'zerop *reals*)
	for (q r) = (multiple-value-list (floor x x))
	unless (and (eql q 1)
		    (zerop r)
		    (if (rationalp x) (eql r 0)
		      (eql r (float 0 x))))
	collect x)
  nil)

(deftest floor.11
  (loop for x in (remove-if #'zerop *reals*)
	for (q r) = (multiple-value-list (floor (- x) x))
	unless (and (eql q -1)
		    (zerop r)
		    (if (rationalp x) (eql r 0)
		      (eql r (float 0 x))))
	collect x)
  nil)

