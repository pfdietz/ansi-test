;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Aug 31 10:48:25 2003
;;;; Contains: Tests of ODDP

(in-package :cl-test)

(deftest oddp.error.1
  (classify-error (oddp))
  program-error)

(deftest oddp.error.2
  (classify-error (oddp 0 nil))
  program-error)

(deftest oddp.1
  (loop for x in *numbers*
	when (integerp x)
	do (oddp x))
  nil)

(deftest oddp.2
  (loop for x in *mini-universe*
	unless (or (integerp x)
		   (eq (classify-error** `(oddp (quote ,x))) 'type-error))
	collect x)
  nil)

(deftest oddp.3
  (let ((upper-bound most-positive-fixnum)
	(lower-bound most-negative-fixnum))
    (loop for x = (- (random (- upper-bound lower-bound)) lower-bound)
	  repeat 10000
	  when (or
		(oddp (+ x x))
		(not (oddp (+ x x 1)))
		(if (oddp x)
		    (or (oddp (1+ x))
			(oddp (1- x))
			(/= (mod x 2) 1))
		  (or (not (oddp (1+ x)))
		      (not (oddp (1- x)))
		      (/= (mod x 2) 0))))
	  collect x))
  nil)

(deftest oddp.4
  (let ((upper-bound 1000000000000000)
	(lower-bound -1000000000000000))
    (loop for x = (- (random (- upper-bound lower-bound)) lower-bound)
	  repeat 10000
	  when (or
		(oddp (+ x x))
		(not (oddp (+ x x 1)))
		(if (oddp x)
		    (or (oddp (1+ x))
			(oddp (1- x))
			(/= (mod x 2) 1))
		  (or (not (oddp (1+ x)))
		      (not (oddp (1- x)))
		      (/= (mod x 2) 0))))
	  collect x))
  nil)

			