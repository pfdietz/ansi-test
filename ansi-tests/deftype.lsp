;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 12:56:56 2003
;;;; Contains: Tests of DEFTYPE

(in-package :cl-test)

(deftest deftype.error.1
  (classify-error (funcall (macro-function 'deftype)))
  program-error)

(deftest deftype.error.2
  (classify-error (funcall (macro-function 'deftype)
			   '(deftype nonexistent-type () nil)))
  program-error)

(deftest deftype.error.3
  (classify-error (funcall (macro-function 'deftype)
			   '(deftype nonexistent-type () nil)
			   nil nil))
  program-error)
