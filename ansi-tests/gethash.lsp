;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Nov 28 06:05:21 2003
;;;; Contains: Tests of GETHASH

(in-package :cl-test)

;;; Most testing of GETHASH is in test-hash-table-1 in hash-table-aux.lsp

(deftest gethash.1
  (gethash 'x (make-hash-table) 'y)
  y nil)

(deftest gethash.2
  (gethash nil (make-hash-table) 'a)
  a nil)

(deftest gethash.3
  (gethash nil (make-hash-table) 'a)
  a nil)

(deftest gethash.4
  (multiple-value-bind (value present)
      (gethash 'a (let ((table (make-hash-table)))
		    (setf (gethash 'a table) 'b)
		    table))
    (values value (notnot present)))
  b t)

(deftest gethash.5
  (let ((table (make-hash-table))
	(i 0))
    (values
     (setf (gethash 'x table (incf i)) 'y)
     i
     (gethash 'x table)))
  y 1 y)

(deftest gethash.error.1
  (classify-error (gethash))
  program-error)

(deftest gethash.error.2
  (classify-error (gethash 'foo))
  program-error)

(deftest gethash.error.3
  (classify-error (gethash 'foo (make-hash-table) nil nil))
  program-error)

