;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Oct 17 23:21:11 2002
;;;; Contains: Tests for IDENTITY

(in-package :cl-test)

(deftest identity.1
  (loop for x in *universe*
	always (eqlt x (identity x)))
  t)

(deftest identity.2
  (let ((x (ash 1 100)))
    (eqlt x (identity x)))
  t)

(deftest identity.3
  (let ((x 1.00000001))
    (eqlt x (identity x)))
  t)
