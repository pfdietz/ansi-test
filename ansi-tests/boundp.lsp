;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan 14 05:58:01 2003
;;;; Contains: Tests for BOUNDP

(in-package :cl-test)

(deftest boundp.error.1
  (classify-error (boundp))
  program-error)

(deftest boundp.error.2
  (classify-error (boundp 'a 'a))
  program-error)

(deftest boundp.error.3
  (classify-error (boundp 1))
  type-error)

(deftest boundp.error.4
  (classify-error (boundp '(setf car)))
  type-error)

(deftest boundp.error.5
  (classify-error (boundp "abc"))
  type-error)

;;; See other tests in cl-symbols.lsp

(deftest boundp.1
  (notnot (boundp 't))
  t)

(deftest boundp.2
  (notnot (boundp nil))
  t)

(deftest boundp.3
  (notnot (boundp :foo))
  t)

(deftest boundp.4
  (boundp '#:foo)
  nil)
