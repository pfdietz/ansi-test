;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Sep 30 19:16:59 2002
;;;; Contains: Tests for class and function STRING

(in-package :cl-test)

(deftest string.1
  (subtypep 'string 'array)
  t t)

(deftest string.2
  (subtypep 'string 'vector)
  t t)

(deftest string.3
  (subtypep 'string 'sequence)
  t t)

(deftest string.4
  (let ((s (string #\a)))
    (values (stringp s) s))
  t "a")

(deftest string.5
  (let ((s (string "")))
    (values (stringp s) s))
  t "")

(deftest string.6
  (let ((s (string '|FOO|)))
    (values (stringp s) s))
  t "FOO")

(deftest string.7
  (loop for x in *universe*
	always (handler-case (stringp (string x))
			     (type-error () :caught)))
  t)


		 

