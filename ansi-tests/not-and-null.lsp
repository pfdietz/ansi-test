;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Oct 17 06:38:33 2002
;;;; Contains: Tests of NOT and NULL

(in-package :cl-test)

(deftest null.1
  (null nil)
  t)

(deftest null.2
  (null t)
  nil)

(deftest null.3
  (some #'(lambda (x) (and x (null x))) *universe*)
  nil)

(deftest not.1
  (not nil)
  t)

(deftest not.2
  (not t)
  nil)

(deftest not.3
  (some #'(lambda (x) (and x (not x))) *universe*)
  nil)


