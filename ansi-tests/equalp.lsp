;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Oct 17 22:14:42 2002
;;;; Contains: Tests for EQUALP

(in-package :cl-test)

(deftest equalp.1
  (loop for c across +base-chars+
	always (loop for d across +base-chars+
		     always (if (char-equal c d) (equalpt c d)
			      (not (equalpt c d)))))
  t)

(deftest equalp.2
  (loop for i from 1 to 100
	always (loop for j from 1 to 100
		     always (if (eqlt i j) (equalpt i j)
			      (not (equalpt i j)))))
  t)

(deftest equalp.3
  (equalpt "abc" "ABC")
  t)

(deftest equalp.4
  (equalpt "abc" "abd")
  nil)

