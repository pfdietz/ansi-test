;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Sep  7 08:36:31 2003
;;;; Contains: Tests of RATIONALP

(in-package :cl-test)

(deftest rationalp.error.1
  (classify-error (rationalp))
  program-error)

(deftest rationalp.error.2
  (classify-error (rationalp 0 nil))
  program-error)

(deftest rationalp.error.3
  (classify-error (rationalp 'a 0))
  program-error)

(deftest rationalp.1
  (loop for x in *rationals*
	for vals = (multiple-value-list (rationalp x))
	unless (and (= (length vals) 1)
		    (first vals))
	collect (cons x vals))
  nil)

(deftest rationalp.2
  (loop for x in (set-difference *universe* *rationals*)
	for vals = (multiple-value-list (rationalp x))
	unless (and (= (length vals) 1)
		    (null (first vals)))
	collect (cons x vals))
  nil)

(deftest rationalp.3
  (loop for x in *universe*
	when (if (typep x 'rational) (not (rationalp x)) (rationalp x))
	collect x)
  nil)


