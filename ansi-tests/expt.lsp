;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Sep  2 19:36:22 2003
;;;; Contains: Tests of EXPT

(in-package :cl-test)

(deftest expt.error.1
  (classify-error (expt))
  program-error)

(deftest expt.error.2
  (classify-error (expt 1 1 1))
  program-error)

(deftest expt.error.3
  (classify-error (expt 1 1 nil nil))
  program-error)

(deftest expt.1
  (expt 0 0)
  1)

(deftest expt.2
  (loop for i from -1000 to 1000
	always (eql (expt i 0) 1))
  t)

(deftest expt.3
  (loop for i = (random 1.0s3)
	repeat 1000
	always (eql (expt i 0) 1.0s0))
  t)

(deftest expt.4
  (loop for i = (random 1.0f6)
	repeat 1000
	always (eql (expt i 0) 1.0f0))
  t)

(deftest expt.5
  (loop for i = (random 1.0d10)
	repeat 1000
	always (eql (expt i 0) 1.0d0))
  t)

(deftest expt.6
  (loop for i = (random 1.0l10)
	repeat 1000
	always (eql (expt i 0) 1.0l0))
  t)

(deftest expt.7
  (loop for i from -1000 to 1000
	for c = (complex i i)
	always (eql (expt c 0) 1))
  t)

(deftest expt.8
  (loop for i = (random 1.0s3)
	for c = (complex i i)
	repeat 1000
	always (eql (expt c 0) #c(1.0s0 0.0s0)))
  t)

(deftest expt.9
  (loop for i = (random 1.0f6)
	for c = (complex i i)
	repeat 1000
	always (eql (expt c 0) #c(1.0f0 0.0f0)))
  t)

(deftest expt.10
  (loop for i = (random 1.0d10)
	for c = (complex i i)
	repeat 1000
	always (eql (expt c 0) #c(1.0d0 0.0d0)))
  t)

(deftest expt.11
  (loop for i = (random 1.0l10)
	for c = (complex i i)
	repeat 1000
	always (eql (expt c 0) #c(1.0l0 0.0l0)))
  t)

(deftest expt.12
  (loop for x in *numbers*
	unless (or (floatp (realpart x))
		   (eql (expt x 1) x))
	collect x)
  nil)

(deftest expt.13
  (loop for x in *rationals*
	unless (and (eql (expt x 2) (* x x))
		    (or (zerop x)
			(eql (expt x -1) (/ x))))
	collect x)
  nil)

(deftest expt.14
  (expt #c(0 2) 2)
  -4)

(deftest expt.15
  (expt #c(1 1) 2)
  #c(0 2))

(deftest expt.16
  (expt #c(1/2 1/3) 3)
  #c(-1/24 23/108))

(deftest expt.17
  (expt #c(1 1) -2)
  #c(0 -1/2))

(deftest expt.18
  (loop
   for zero in '(0.0s0 0.0f0 0.0d0 0.0l0)
   always
   (loop for i from -1000 to 1000
	 always (eql (expt i zero) (float 1 zero))))
  t)

(deftest expt.19
  (loop
   for zero in '(0.0s0 0.0f0 0.0d0 0.0l0)
   always
   (loop for i from -1000 to 1000
	 always (eql (expt (float i 0.0s0) zero) (float 1 zero))))
  t)

(deftest expt.20
  (loop
   for zero in '(0.0f0 0.0d0 0.0l0)
   always
   (loop for i from -1000 to 1000
	 always (eql (expt (float i 0.0f0) zero) (float 1 zero))))
  t)

(deftest expt.21
  (loop
   for zero in '(0.0d0 0.0l0)
   always
   (loop for i from -1000 to 1000
	 always (eql (expt (float i 0.0d0) zero) (float 1 zero))))
  t)

(deftest expt.22
  (expt 2.0f0 0.0s0)
  1.0f0)

(deftest expt.23
  (expt 2.0d0 0.0s0)
  1.0d0)

(deftest expt.24
  (expt 2.0l0 0.0s0)
  1.0l0)

(deftest expt.25
  (expt 2.0d0 0.0f0)
  1.0d0)

(deftest expt.26
  (expt 2.0l0 0.0f0)
  1.0l0)

(deftest expt.27
  (expt 2.0l0 0.0d0)
  1.0l0)





	

