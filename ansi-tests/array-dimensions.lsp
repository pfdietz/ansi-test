;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan 21 06:59:37 2003
;;;; Contains: Tests of ARRAY-DIMENSIONS

(in-package :cl-test)

;;; The tests in make-array.lsp also test this function

(deftest array-dimensions.1
  (array-dimensions #0aX)
  nil)

(deftest array-dimensions.2
  (array-dimensions #(a b c d))
  (4))

(deftest array-dimensions.3
  (array-dimensions #*0011011011)
  (10))

(deftest array-dimensions.4
  (array-dimensions "abcdef")
  (6))

(deftest array-dimensions.5
  (array-dimensions #2a((1 2 3)(4 5 6)(7 8 9)(10 11 12)))
  (4 3))

(deftest array-dimensions.6
  (let ((a (make-array '(2 3 4) :adjustable t)))
    (values (array-dimension a 0)
	    (array-dimension a 1)
	    (array-dimension a 2)))
  2 3 4)

(deftest array-dimensions.7
  (let ((a (make-array '(10) :fill-pointer 5)))
    (array-dimension a 0))
  10)

;;; Error tests

(deftest array-dimensions.error.1
  (classify-error (array-dimensions))
  program-error)

(deftest array-dimensions.error.2
  (classify-error (array-dimensions #(a b c) nil))
  program-error)

(deftest array-dimensions.error.3
  (let (why)
    (loop for e in *mini-universe*
	  unless (or (typep e 'array)
		     (eq 'type-error
			 (setq why (classify-error**
				    `(array-dimensions ',e)))))
	  collect (list e why)))
  nil)

(deftest array-dimensions.error.4
  (classify-error (array-dimensions nil))
  type-error)

(deftest array-dimensions.error.5
  (classify-error (locally (array-dimensions nil)))
  type-error)

