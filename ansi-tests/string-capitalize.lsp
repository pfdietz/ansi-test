;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Oct  3 20:08:26 2002
;;;; Contains: Tests for STRING-CAPITALIZE

(in-package :cl-test)

(deftest string-capitalize.1
  (let ((s "abCd"))
    (values (string-capitalize s) s))
  "Abcd"
  "abCd")


(deftest string-capitalize.2
  (let ((s "0adA2Cdd3wXy"))
    (values (string-capitalize s) s))
  "0adA2Cdd3wXy"
  "0ada2cdd3wxy")

(deftest string-capitalize.3
  (let ((s "1a"))
    (values (string-capitalize s) s))
  "1a"
  "1a")

(deftest string-capitalize.4
  (let ((s "a1a"))
    (values (string-capitalize s) s))
  "A1a"
  "a1a")


