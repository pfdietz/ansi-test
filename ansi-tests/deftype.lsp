;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 12:56:56 2003
;;;; Contains: Tests of DEFTYPE

(in-package :cl-test)

;;; FIXME
;;; Need to add non-error tests

(deftest deftype.error.1
  (signals-error (funcall (macro-function 'deftype))
		 program-error)
  t)

(deftest deftype.error.2
  (signals-error (funcall (macro-function 'deftype)
			   '(deftype nonexistent-type () nil))
		 program-error)
  t)

(deftest deftype.error.3
  (signals-error (funcall (macro-function 'deftype)
			   '(deftype nonexistent-type () nil)
			   nil nil)
		 program-error)
  t)

