;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Sep 11 20:13:22 2003
;;;; Contains: Tests of BYTE, BYTE-SIZE, and BYTE-POSITION

(in-package :cl-test)

(deftest byte.error.1
  (classify-error (byte))
  program-error)

(deftest byte.error.2
  (classify-error (byte 1))
  program-error)

(deftest byte.error.3
  (classify-error (byte 1 1 nil))
  program-error)

(deftest byte.1
  (progn (byte 0 0) :good)
  :good)

(deftest byte.2
  (progn (byte 1 1) :good)
  :good)

(deftest byte.3
  (loop for i from 0 to 100
	always
	(loop for j from 0 to 100
	      always
	      (let ((bspec (byte i j)))
		(and (eql i (byte-size bspec))
		     (eql j (byte-position bspec))))))
  t)

(deftest byte-position.error.1
  (classify-error (byte-position))
  program-error)

(deftest byte-position.error.2
  (classify-error (byte-position (byte 1 1) nil))
  program-error)

(deftest byte-size.error.1
  (classify-error (byte-size))
  program-error)

(deftest byte-size.error.2
  (classify-error (byte-size (byte 1 1) nil))
  program-error)
