;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Sep  6 18:42:15 2003
;;;; Contains: Tests of CIS

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")

(deftest cis.error.1
  (classify-error (cis))
  program-error)

(deftest cis.error.2
  (classify-error (cis 0 nil))
  program-error)

(deftest cis.1
  (eqlt (cis 0) #c(1.0 0.0))
  t)

(deftest cis.2
  (loop for x in '(0.0s0 0.0f0 0.0d0 0.0l0)
	for vals = (multiple-value-list (cis x))
	for c = (car vals)
	unless (and (= (length vals) 1)
		    (eql c (complex (float 1 x) x)))
	collect (cons x vals))
  nil)

(deftest cis.3
  (loop for x = (random (* 2 pi))
	for c = (cis x)
	repeat 1000
	unless (and (complexp c)
		    (approx= (imagpart c) (sin x))
		    (approx= (realpart c) (cos x)))
	collect (list x c (cos x) (sin x)))
  nil)

(deftest cis.4
  (loop for x = (random (coerce (* 2 pi) 'single-float))
	for c = (cis x)
	repeat 1000
	unless (and (complexp c)
		    (approx= (imagpart c) (sin x))
		    (approx= (realpart c) (cos x)))
	collect (list x c (cos x) (sin x)))
  nil)
