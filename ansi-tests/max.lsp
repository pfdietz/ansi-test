;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Aug  3 15:55:17 2003
;;;; Contains: Tests of MAX

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")

(deftest max.error.1
  (classify-error (max))
  program-error)

(deftest max.error.2
  (loop for x in *mini-universe*
	unless (or (realp x)
		   (eq (classify-error** `(classify-error (max ',x)))
		       'type-error))
	collect x)
  nil)

(deftest max.1
  (loop for n in *reals*
	when (or (not (eql (max n) n))
		 (not (eql (max n n) n))
		 (not (eql (max n n n) n))
		 (not (eql (apply #'max (make-list
					 (min 256 call-arguments-limit)
					 :initial-element n))
			   n)))
	collect n)
  nil)

(deftest max.2
  (max.2-fn)
  nil)
