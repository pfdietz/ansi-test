;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Sep 11 23:22:46 2003
;;;; Contains: Tests for LOGTEST

(in-package :cl-test)

(deftest logtest.error.1
  (classify-error (logtest))
  program-error)

(deftest logtest.error.2
  (classify-error (logtest 0))
  program-error)

(deftest logtest.error.3
  (classify-error (logtest 0 0 nil))
  program-error)

(deftest logtest.error.4
  (loop for x in *mini-universe*
	unless (or (integerp x)
		   (eq (eval `(classify-error (logtest ',x -1))) 'type-error))
	collect x)
  nil)

(deftest logtest.error.5
  (loop for x in *mini-universe*
	unless (or (integerp x)
		   (eq (eval `(classify-error (logtest -1 ',x))) 'type-error))
	collect x)
  nil)

(deftest logtest.1
  (loop for x = (logand (random-fixnum) (random-fixnum))
	for y = (logand (random-fixnum) (random-fixnum))
	repeat 10000
	unless (if (logtest x y)
		   (not (zerop (logand x y)))
		 (zerop (logand x y)))
	collect (list x y))
  nil)

(deftest logtest.2
  (logtest 1 2)
  nil)

(deftest logtest.3
  (notnot-mv (logtest 8 (logior 8 4)))
  t)
  

  
