;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Aug 31 10:39:01 2003
;;;; Contains: Tests of EVENP

(in-package :cl-test)

(deftest evenp.error.1
  (classify-error (evenp))
  program-error)

(deftest evenp.error.2
  (classify-error (evenp 0 nil))
  program-error)

(deftest evenp.1
  (loop for x in *numbers*
	when (integerp x)
	do (evenp x))
  nil)

(deftest evenp.2
  (loop for x in *mini-universe*
	unless (or (integerp x)
		   (eq (classify-error** `(evenp (quote ,x))) 'type-error))
	collect x)
  nil)

(deftest evenp.3
  (let ((upper-bound most-positive-fixnum)
	(lower-bound most-negative-fixnum))
    (loop for x = (- (random (- upper-bound lower-bound)) lower-bound)
	  repeat 10000
	  when (or
		(not (evenp (+ x x)))
		(evenp (+ x x 1))
		(if (evenp x)
		    (or (evenp (1+ x))
			(evenp (1- x))
			(/= (mod x 2) 0))
		  (or (not (evenp (1+ x)))
		      (not (evenp (1- x)))
		      (= (mod x 2) 0))))
	  collect x))
  nil)

(deftest evenp.4
  (let ((upper-bound 1000000000000000)
	(lower-bound -1000000000000000))
    (loop for x = (- (random (- upper-bound lower-bound)) lower-bound)
	  repeat 10000
	  when (or
		(not (evenp (+ x x)))
		(evenp (+ x x 1))
		(if (evenp x)
		    (or (evenp (1+ x))
			(evenp (1- x))
			(/= (mod x 2) 0))
		  (or (not (evenp (1+ x)))
		      (not (evenp (1- x)))
		      (= (mod x 2) 0))))
	  collect x))
  nil)

(deftest evenp.5
  (notnot-mv (evenp 0))
  t)

(deftest evenp.6
  (evenp 1)
  nil)

(deftest evenp.7
  (notnot-mv (evenp 100000000000000000000000000000000))
  t)

(deftest evenp.8
  (evenp 100000000000000000000000000000001)
  nil)
