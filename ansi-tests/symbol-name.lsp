;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jun 14 05:45:55 2003
;;;; Contains: Tests of SYMBOL-NAME

(in-package :cl-test)

(deftest symbol-name.1
  (symbol-name '|ABCD|)
  "ABCD")

(deftest symbol-name.2
  (symbol-name '|1234abcdABCD|)
  "1234abcdABCD")

(deftest symbol-name.3
  (signals-error (symbol-name 1) type-error)
  t)

(deftest symbol-name.4
  (signals-error (symbol-name '(a)) type-error)
  t)

(deftest symbol-name.5
  (signals-error (symbol-name "ABCDE") type-error)
  t)

(deftest symbol-name.6
  (signals-error (symbol-name 12913.0213) type-error)
  t)

(deftest symbol-name.7
  (symbol-name :|abcdefg|)
  "abcdefg")

;;; Error tests

(deftest symbol-name.error.1
  (signals-error (symbol-name) program-error)
  t)

(deftest symbol-name.error.2
  (signals-error (symbol-name 'a 'b) program-error)
  t)

(deftest symbol-name.error.3
  (loop for x in *mini-universe*
	for form = `(signals-error (symbol-name ',x) type-error)
	unless (or (symbolp x) (eval form))
	collect x)
  nil)


