;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan 21 22:01:09 2003
;;;; Contains: Tests of ARRAY-TOTAL-SIZE

(in-package :cl-test)

;;; More tests of ARRAY-TOTAL-SIZE are in make-array.lsp

(deftest array-total-size.1
  (array-total-size #0aNIL)
  1)

(deftest array-total-size.2
  (array-total-size "abcdef")
  6)

(deftest array-total-size.3
  (array-total-size #(a b c))
  3)

(deftest array-total-size.4
  (array-total-size #*0011010)
  7)

(deftest array-total-size.5
  (array-total-size #2a((1 2 3)(4 5 6)(7 8 9)(a b c)))
  12)

;;; Error tests

(deftest array-total-size.error.1
  (classify-error (array-total-size))
  program-error)

(deftest array-total-size.error.2
  (classify-error (array-total-size #(a b c) nil))
  program-error)

(deftest array-total-size.error.3
  (loop for e in *mini-universe*
	when (and (not (typep e 'array))
		  (not (eql (classify-error** `(array-total-size ',e))
			    'type-error)))
	collect e)
  nil)

(deftest array-total-size.error.4
  (classify-error (array-total-size 0))
  type-error)










