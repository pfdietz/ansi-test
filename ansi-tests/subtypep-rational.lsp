;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Feb 15 11:56:19 2003
;;;; Contains: Tests for subtype relationships on rational types

(in-package :cl-test)

;;; SUBTYPEP on rational types

(deftest subtypep.rational.1
  (subtypep* '(rational 10 *) 'rational)
  t t)

(deftest subtypep.rational.2
  (subtypep* '(rational 10) 'rational)
  t t)

(deftest subtypep.rational.3
  (subtypep* '(rational * 10) 'rational)
  t t)

(deftest subtypep.rational.4
  (subtypep '(rational 10 20) 'rational)
  t t)

(deftest subtypep.rational.5
  (subtypep '(rational (10) 20) 'rational)
  t t)

(deftest subtypep.rational.6
  (subtypep '(rational 10 (20)) 'rational)
  t t)

(deftest subtypep.rational.7
  (subtypep '(rational (10) (20)) 'rational)
  t t)



