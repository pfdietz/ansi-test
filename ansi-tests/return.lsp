;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 16:00:32 2003
;;;; Contains: Tests of RETURN

(in-package :cl-test)

(deftest return.error.1
  (classify-error (funcall (macro-function 'return)))
  program-error)
  
(deftest return.error.2
  (classify-error (funcall (macro-function 'return)
			   '(return nil)))
  program-error)

(deftest return.error.3
  (classify-error (funcall (macro-function 'return)
			   '(return nil)
			   nil nil))
  program-error)
  
