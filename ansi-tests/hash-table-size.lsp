;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Nov 28 05:23:45 2003
;;;; Contains: Tests for HASH-TABLE-SIZE

(in-package :cl-test)

(deftest hash-table-size.error.1
  (classify-error (hash-table-size))
  program-error)

(deftest hash-table-size.error.2
  (classify-error (hash-table-size (make-hash-table) nil))
  program-error)

(deftest hash-table-size.error.3
  (loop for x in *mini-universe*
	unless (hash-table-p x)
	unless (eq (eval `(classify-error (hash-table-size ',x)))
		   'type-error)
	collect x)
  nil)
