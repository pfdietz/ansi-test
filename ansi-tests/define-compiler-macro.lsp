;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 12:33:02 2003
;;;; Contains: Tests of DEFINE-COMPILER-MACRO

(in-package :cl-test)

;;; Need to add non-error tests

(deftest define-compiler-macro.error.1
  (signals-error (funcall (macro-function 'define-compiler-macro))
		 program-error)
  t)

(deftest define-compiler-macro.error.2
  (signals-error (funcall (macro-function 'define-compiler-macro)
			   '(definee-compiler-macro nonexistent-function ()))
		 program-error)
  t)

(deftest define-compiler-macro.error.3
  (signals-error (funcall (macro-function 'define-compiler-macro)
			   '(definee-compiler-macro nonexistent-function ())
			   nil nil)
		 program-error)
  t)
