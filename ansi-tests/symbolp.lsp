;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jun 22 08:59:12 2003
;;;; Contains: Tests for SYMBOLP

(in-package :cl-test)

(deftest symbolp.1
  (notnot-mv (symbolp nil))
  t)

(deftest symbolp.2
  (loop for x in *symbols*
	unless (symbolp x)
	collect x)
  nil)

(deftest symbolp.3
  (loop for x in (set-difference *universe* *symbols*)
	when (symbolp x)
	collect x)
  nil)

;;; Error cases

(deftest symbolp.error.1
  (classify-error (symbolp))
  program-error)

(deftest symbolp.error.2
  (classify-error (symbolp nil nil))
  program-error)
