;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Aug  4 21:33:44 2003
;;;; Contains: Tests of MINUSP

(in-package :cl-test)

(deftest minusp.error.1
  (classify-error (minusp))
  program-error)

(deftest minusp.error.2
  (classify-error (minusp 0 0))
  program-error)

(deftest minusp.error.3
  (classify-error (minusp 0 nil))
  program-error)

(deftest minusp.error.4
  (loop for x in *mini-universe*
	unless (realp x)
	unless (eql (classify-error** `(minusp ',x)) 'type-error)
	collect x)
  nil)

(deftest minusp.1
  (minusp 0)
  nil)

(deftest minusp.2
  (notnot-mv (minusp -1))
  t)

(deftest minusp.3
  (minusp 1)
  nil)

(deftest minusp.4
  (loop for x in *reals*
	when (if (minusp x) (>= x 0) (< x 0))
	collect x)
  nil)

(deftest minusp.5
  (some #'minusp '(-0.0s0 -0.0f0 -0.0d0 -0.0l0))
  nil)

(deftest minusp.6
  (remove-if #'minusp
	     (list least-negative-short-float
		   least-negative-normalized-short-float
		   least-negative-single-float
		   least-negative-normalized-single-float
		   least-negative-double-float
		   least-negative-normalized-double-float
		   least-negative-long-float
		   least-negative-normalized-long-float
		   most-negative-short-float
		   most-negative-single-float
		   most-negative-double-float
		   most-negative-long-float))
  nil)
