;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon May 23 07:13:32 2005
;;;; Contains: Tests of TYPEP

(in-package :cl-test)

(deftest typep.error.1
  (signals-error (typep) program-error)
  t)

(deftest typep.error.2
  (signals-error (typep nil) program-error)
  t)

(deftest typep.error.3
  (signals-error (typep nil t nil nil) program-error)
  t)

(deftest typep.error.4
  (signals-error-always (typep nil 'values) error)
  t t)

(deftest typep.error.5
  (signals-error-always (typep nil '(values)) error)
  t t)

(deftest typep.error.6
  (signals-error-always (typep nil '(values t t t t)) error)
  t t)

(deftest typep.error.7
  (signals-error-always (typep nil '(function () t)) error)
  t t)

