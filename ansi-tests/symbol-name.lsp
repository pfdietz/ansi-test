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
  (classify-error (symbol-name 1))
  type-error)

(deftest symbol-name.4
  (classify-error (symbol-name '(a)))
  type-error)

(deftest symbol-name.5
  (classify-error (symbol-name "ABCDE"))
  type-error)

(deftest symbol-name.6
  (classify-error (symbol-name 12913.0213))
  type-error)

(deftest symbol-name.7
  (symbol-name :|abcdefg|)
  "abcdefg")

(deftest symbol-name.error.1
  (classify-error (symbol-name))
  program-error)

(deftest symbol-name.error.2
  (classify-error (symbol-name 'a 'b))
  program-error)

