;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Sep  7 08:22:06 2003
;;;; Contains: Tests of REALP

(in-package :cl-test)

(deftest realp.error.1
  (classify-error (realp))
  program-error)

(deftest realp.error.2
  (classify-error (realp 0 nil))
  program-error)

(deftest realp.error.3
  (classify-error (realp nil nil))
  program-error)

(deftest realp.1
  (notnot-mv (realp 0))
  t)
  
(deftest realp.2
  (notnot-mv (realp 0.0))
  t)
  
(deftest realp.3
  (realp #c(1 2))
  nil)

(deftest realp.4
  (notnot-mv (realp 17/13))
  t)

(deftest realp.5
  (realp 'a)
  nil)

(deftest realp.6
  (loop for x in *universe*
	for vals = (multiple-value-list (realp x))
	for p = (car vals)
	when (or (/= (length vals) 1)
		 (if (typep x 'real) (not p) p))
	collect (cons x vals))
  nil)


