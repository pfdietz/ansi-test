;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Sep  6 19:56:29 2003
;;;; Contains: Tests of COMPLEX

(in-package :cl-test)

(deftest complex.error.1
  (classify-error (complex))
  program-error)

(deftest complex.error.2
  (classify-error (complex 1 1 nil))
  program-error)

(deftest complex.1
  (loop for x in *rationals*
	for c = (complex x)
	always (eql c x))
  t)

(deftest complex.2
  (loop for x in *floats*
	for c = (complex x)
	always (and (complexp c)
		    (eql x (realpart c))
		    (eql (float 0 x) (imagpart c))))
  t)

(deftest complex.3
  (loop for x in *rationals*
	for c = (complex 0 x)
	unless (or (zerop x)
		   (and (complexp c)
			(eql (realpart c) 0)
			(eql (imagpart c) x)))
	collect (list c x))
  nil)

(deftest complex.4
  (loop for x in *floats*
	for c = (complex 0 x)
	always (and (complexp c)
		    (eql (float 0 x) (realpart c))
		    (eql x (imagpart c))))
  t)
