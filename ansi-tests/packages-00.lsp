;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 25 08:07:31 1998
;;;; Contains: Package test code (common code)

(in-package :cl-test)
(declaim (optimize (safety 3)))
 
(defpackage "A"
  (:use)
  (:nicknames "Q")
  (:export "FOO"))

(defpackage "B"
  (:use "A")
  (:export "BAR"))

(defpackage "DS1"
  (:use)
  (:intern "C" "D")
  (:export "A" "B"))

(defpackage "DS2"
  (:use)
  (:intern "E" "F")
  (:export "G" "H" "A"))

(defpackage "DS3"
  (:shadow "B")
  (:shadowing-import-from "DS1" "A")
  (:use "DS1" "DS2")
  (:export "A" "B" "G" "I" "J" "K")
  (:intern "L" "M"))

(defpackage "DS4"
  (:shadowing-import-from "DS1" "B")
  (:use "DS1" "DS3")
  (:intern "X" "Y" "Z")
  (:import-from "DS2" "F"))
