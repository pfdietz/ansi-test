;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jun 15 12:05:47 2003
;;;; Contains: Tests of CLASS-NAME

(in-package :cl-test)

;;; This is mostly tested elsewhere.

(deftest class-name.1
  (class-name (find-class 'symbol))
  symbol)

(defclass class-name-class-01 () (a b c))

(report-and-ignore-errors
 (eval '(defmethod class-name ((x class-name-class-01)) 'silly)))

(deftest class-name.2
  (class-name (make-instance 'class-name-class-01))
  silly)

;; Tests of (setf class-name)

(deftest setf-class-name.1
  (typep* #'(setf class-name) 'standard-generic-function)
  t)

(deftest class-name.error.1
  (classify-error (class-name))
  program-error)

(deftest class-name.error.2
  (classify-error (class-name (find-class 'symbol) nil))
  program-error)
