;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 12:35:24 2003
;;;; Contains: Tests of DEFMACRO

(in-package :cl-test)

;;; Need to add non-error tests

(deftest defmacro.error.1
  (classify-error (funcall (macro-function 'defmacro)))
  program-error)

(deftest defmacro.error.2
  (classify-error (funcall (macro-function 'defmacro)
			   '(defmacro nonexistent-macro ())))
  program-error)

(deftest defmacro.error.3
  (classify-error (funcall (macro-function 'defmacro)
			   '(defmacro nonexistent-macro ())
			   nil nil))
  program-error)
