;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Feb 24 20:22:23 2004
;;;; Contains: Tests of RETURN-FROM

(in-package :cl-test)

;;; RETURN-FROM is tested extensively in other files

(deftest return-from.error.1
  (signals-error (funcall (macro-function 'return-from)) program-error)
  t)
  
(deftest return-from.error.2
  (signals-error (funcall (macro-function 'return-from) '(return-from nil nil))
		 program-error)
  t)

(deftest return-from.error.3
  (signals-error (funcall (macro-function 'return-from)
			  '(return-from nil) nil nil)
		 program-error)
  t)

;;;

(deftest return-from.1
  (block xyz (return-from xyz) :bad)
  nil)

(deftest return-from.2
  (block nil (return-from nil :good) :bad)
  :good)


