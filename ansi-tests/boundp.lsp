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

(deftest boundp.error.6
  (classify-error (locally (boundp "abc") t))
  type-error)

;;; See other tests in cl-symbols.lsp

(deftest boundp.1
  (notnot-mv (boundp 't))
  t)

(deftest boundp.2
  (notnot-mv (boundp nil))
  t)

(deftest boundp.3
  (notnot-mv (boundp :foo))
  t)

(deftest boundp.4
  (boundp '#:foo)
  nil)

(deftest boundp.order.1
  (let ((i 0) x)
    (values
     (boundp (progn (setf x (incf i)) '#:foo))
     i x))
  nil 1 1)

