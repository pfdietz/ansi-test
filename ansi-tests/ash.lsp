;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Sep  7 08:43:03 2003
;;;; Contains: Tests of ASH

(in-package :cl-test)

(deftest ash.error.1
  (classify-error (ash))
  program-error)

(deftest ash.error.2
  (classify-error (ash 1 1 1))
  program-error)

(deftest ash.error.3
  (classify-error (ash 1 1 nil))
  program-error)

(deftest ash.error.4
  (loop for x in *mini-universe*
	unless (or (integerp x)
		   (eq (eval `(classify-error (ash ',x 0))) 'type-error))
	collect x)
  nil)

(deftest ash.error.5
  (loop for x in *mini-universe*
	unless (or (integerp x)
		   (eq (eval `(classify-error (ash 0 ',x))) 'type-error))
	collect x)
  nil)

(deftest ash.1
  (loop for x in *integers*
	always (eql (ash x 0) x))
  t)

(deftest ash.2
  (loop for i = (random-fixnum)
	for s = (random-from-interval 40)
	for ishifted = (ash i s)
	repeat 1000
	always (eql (floor (* i (expt 2 s))) ishifted))
  t)

(deftest ash.3
  (let* ((nbits 100)
	 (bound (expt 2 nbits)))
    (loop for i = (random-from-interval bound)
	  for s = (random-from-interval (+ nbits 20))
	  for ishifted = (ash i s)
	  repeat 1000
	  always (eql (floor (* i (expt 2 s))) ishifted)))
  t)

(deftest ash.order.1
  (let ((i 0) x y)
    (values (ash (progn (setf x (incf i)) 1)
		 (progn (setf y (incf i)) 2))
	    i x y))
  4 2 1 2)
