;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Nov 21 10:43:15 2002
;;;; Contains: Tests of EVAL

(in-package :cl-test)

(deftest eval.1
  (eval 1)
  1)

(deftest eval.2
  (loop for x being the symbols of "KEYWORD"
	always (eq (eval x) x))
  t)

(deftest eval.3
  (let ((s "abcd"))
    (eqlt (eval s) s))
  t)

(deftest eval.4
  (eval '(car '(a . b)))
  a)

(deftest eval.5
  (eval '(let ((x 0)) x))
  0)

(deftest eval.6
  (funcall #'eval 1)
  1)

(deftest eval.order.1
  (let ((i 0))
    (values (eval (progn (incf i) 10)) i))
  10 1)

;;; Error cases

(deftest eval.error.1
  (classify-error (eval))
  program-error)

(deftest eval.error.2
  (classify-error (eval nil nil))
  program-error)

(deftest eval.error.3
  (classify-error (eval (list (gensym))))
  undefined-function)

