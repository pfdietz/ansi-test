;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 12:35:24 2003
;;;; Contains: Tests of DEFMACRO

(in-package :cl-test)

;;; FIXME
;;; Need to add non-error tests

(deftest defmacro.error.1
  (signals-error (funcall (macro-function 'defmacro))
		 program-error)
  t)

(deftest defmacro.error.2
  (signals-error (funcall (macro-function 'defmacro)
			   '(defmacro nonexistent-macro ()))
		 program-error)
  t)

(deftest defmacro.error.3
  (signals-error (funcall (macro-function 'defmacro)
			   '(defmacro nonexistent-macro ())
			   nil nil)
		 program-error)
  t)
