;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Sep 11 23:07:33 2003
;;;; Contains: Tests of FLOATP

(in-package :cl-test)

(deftest floatp.error.1
  (classify-error (floatp))
  program-error)

(deftest floatp.error.2
  (classify-error (floatp 1.0 nil))
  program-error)

(deftest floatp.1
  (notnot-mv (floatp 1.0))
  t)

(deftest floatp.2
  (floatp nil)
  nil)

(deftest floatp.3
  (loop for x in *universe*
	for vals = (multiple-value-list (floatp x))
	unless (and (= (length vals) 1)
		    (if (car vals)
			(typep x 'float)
		      (not (typep x 'float))))
	collect x)
  nil)

