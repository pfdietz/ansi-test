;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan 21 20:32:57 2003
;;;; Contains: Tests for ARRAY-RANK

(in-package :cl-test)

;;; Most tests for ARRAY-RANK are in make-array.lsp

(deftest array-rank.1
  (array-rank #0aNIL)
  0)

(deftest array-rank.2
  (loop for e in *universe*
	when (and (typep e 'vector)
		  (not (eql (array-rank e) 1)))
	collect e)
  nil)

(deftest array-rank.order.1
  (let ((i 0) a)
    (values
     (array-rank (progn (setf a (incf i)) "abcd"))
     i a))
  1 1 1)

;;; Error tests

(deftest array-rank.error.1
  (signals-error (array-rank) program-error)
  t)

(deftest array-rank.error.2
  (signals-error (array-rank #(a b c) nil)  program-error)
  t)

(deftest array-rank.error.3
  (check-type-error #'array-rank #'arrayp)
  nil)

(deftest array-rank.error.4
  (signals-error (array-rank nil) type-error)
  t)

(deftest array-rank.error.5
  (signals-type-error x nil (locally (array-rank x) t))
  t)
