;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Sep  6 18:20:36 2003
;;;; Contains: Tests of NUMBERP

(in-package :cl-test)

(deftest numberp.error.1
  (signals-error (numberp) program-error)
  t)

(deftest numberp.error.2
  (signals-error (numberp 0 nil) program-error)
  t)

(deftest numberp.error.3
  (signals-error (numberp 'a nil nil) program-error)
  t)

(deftest numberp.1
  (loop for x in *universe*
	for vals = (multiple-value-list (numberp x))
	for val = (car vals)
	unless (and (= (length vals) 1)
		    (if val (typep x 'number)
		      (not (typep x 'number))))
	collect (cons x vals))
  nil)
