;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Nov 27 06:43:21 2002
;;;; Contains: Tests of LAMBDA forms

(in-package :cl-test)

(deftest lambda.1
  ((lambda (x) x) 'a)
  a)

(deftest lambda.2
  ((lambda () 'a))
  a)

(deftest lambda.3
  ((lambda () "documentation" 'a))
  a)

(deftest lambda.4
  ((lambda (x) (declare (type symbol x)) x) 'z)
  z)

(deftest lambda.5
  ((lambda (&aux (x 'a)) x))
  a)

(deftest lambda.6
  ((lambda (&aux (x 'a)) (declare (type symbol x)) x))
  a)

(deftest lambda.7
  ((lambda () "foo"))
  "foo")

(deftest lambda.8
  ((lambda () "foo" "bar"))
  "bar")

(deftest lambda.9
  ((lambda (x y) (declare (ignore x)) "foo" (declare (ignore y)) "bar") 1 2)
  "bar")

(deftest lambda.10
  ((lambda (x) (declare (type symbol x))) 'z)
  nil)

;;; Should test lambda argument lists more fully here

;;; Tests of lambda as a macro

(deftest lambda.macro.1
  (notnot (macro-function 'lambda))
  t)

(deftest lambda.macro.2
  (funcall (eval (macroexpand '(lambda () 10))))
  10)

  