;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Sep  6 21:03:18 2003
;;;; Contains: Tests for COMPLEXP

(in-package :cl-test)

(deftest complexp.error.1
  (signals-error (complexp) program-error)
  t)

(deftest complexp.error.2
  (signals-error (complexp 0 0) program-error)
  t)

(deftest complexp.error.3
  (signals-error (complexp #C(1 1) nil) program-error)
  t)

(deftest complexp.1
  (loop for x in *universe*
	for vals = (multiple-value-list (complexp x))
	when (or (/= (length vals) 1)
		 (if (typep x 'complex)
		     (not (car vals))
		   (car vals)))
	collect (cons x vals))
  nil)
