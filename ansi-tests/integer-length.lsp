;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Sep  7 10:10:10 2003
;;;; Contains: Tests for INTEGER-LENGTH

(in-package :cl-test)

(deftest integer-length.error.1
  (classify-error (integer-length))
  program-error)

(deftest integer-length.error.2
  (classify-error (integer-length 1 1))
  program-error)

(deftest integer-length.error.3
  (classify-error (integer-length 1 nil))
  program-error)

(deftest integer-length.error.4
  (loop for x in *mini-universe*
	unless (or (integerp x)
		   (eq (eval `(classify-error (integer-length ',x)))
		       'type-error))
	collect x)
  nil)

(deftest integer-length.1
  (loop for len from 0 to 100
	for i = (1- (ash 1 len))
	for vals = (multiple-value-list (integer-length i))
	for len2 = (car vals)
	always (and (= (length vals) 1)
		    (eql len len2)))
  t)

(deftest integer-length.2
  (loop for len from 0 to 100
	for i = (ash 1 len)
	for vals = (multiple-value-list (integer-length i))
	for len2 = (car vals)
	always (and (= (length vals) 1)
		    (eql (1+ len) len2)))
  t)

(deftest integer-length.3
  (loop for len from 0 to 100
	for i = (- (ash 1 len))
	for vals = (multiple-value-list (integer-length i))
	for len2 = (car vals)
	always (and (= (length vals) 1)
		    (eql len len2)))
  t)

(deftest integer-length.4
  (loop for len from 0 to 100
	for i = (- -1 (ash 1 len))
	for vals = (multiple-value-list (integer-length i))
	for len2 = (car vals)
	always (and (= (length vals) 1)
		    (eql (1+ len) len2)))
  t)
