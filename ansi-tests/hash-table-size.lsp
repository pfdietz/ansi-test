;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Nov 28 05:23:45 2003
;;;; Contains: Tests for HASH-TABLE-SIZE

(in-package :cl-test)

(deftest hash-table-size.error.1
  (signals-error (hash-table-size) program-error)
  t)

(deftest hash-table-size.error.2
  (signals-error (hash-table-size (make-hash-table) nil)
		 program-error)
  t)

(deftest hash-table-size.error.3
  (loop for x in *mini-universe*
	unless (hash-table-p x)
	unless (eval `(signals-error (hash-table-size ',x)
				     type-error))
	collect x)
  nil)
