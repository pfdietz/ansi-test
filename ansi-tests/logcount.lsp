;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Sep 11 23:12:56 2003
;;;; Contains: Tests of LOGCOUNT

(in-package :cl-test)

(deftest logcount.error.1
  (classify-error (logcount))
  program-error)

(deftest logcount.error.2
  (classify-error (logcount 0 nil))
  program-error)

(deftest logcount.error.3
  (loop for x in *mini-universe*
	unless (or (integerp x)
		   (eq (eval `(classify-error (logcount ',x))) 'type-error))
	collect x)
  nil)

(deftest logcount.1
  (logcount 0)
  0)

(deftest logcount.2
  (logcount 1)
  1)

(deftest logcount.3
  (logcount 2)
  1)

(deftest logcount.4
  (logcount 3)
  2)

(deftest logcount.5
  (logcount -1)
  0)

(deftest logcount.6
  (loop for x = (random-fixnum)
	repeat 100
	always (eql (logcount x) (logcount (lognot x))))
  t)

(deftest logcount.7
  (let ((bound (ash 1 300)))
    (loop for x = (random-from-interval bound)
	  repeat 100
	  always (eql (logcount x) (logcount (lognot x)))))
  t)

(deftest logcount.8
  (loop for y = (random (1+ most-positive-fixnum))
	repeat 100
	unless
	(let ((cnt 0)
	      (x y))
	  (loop while (> x 0)
		do
		(when (oddp x) (incf cnt))
		(setf x (ash x -1)))
	  (eql cnt (logcount y)))
	collect y)
  nil)


