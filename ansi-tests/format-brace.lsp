;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Aug 25 22:08:51 2004
;;;; Contains: Tests of the ~"{ ... ~} format directives

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(deftest format.{.1
  (format nil "~{~
~}" nil)
  "")

(deftest format.{.1a
  (format nil "~{~}" "" nil)
  "")

(deftest format.{.1b
  (format nil "~0{~}" "" '(1 2 3))
  "")

(deftest format.{.2
  (format nil "~{ ~}" nil)
  "")

(deftest format.{.3
  (format nil "~{X Y Z~}" nil)
  "")

(deftest format.{.4
  (format nil "~{~A~}" '(1 2 3 4))
  "1234")

(deftest format.{.5
  (format nil "~{~{~A~}~}" '((1 2 3)(4 5)(6 7 8)))
  "12345678")

(deftest format.{.6
  (format nil "~{~1{~A~}~}" '((1 2 3)(4 5)(6 7 8)))
  "146")

(deftest format.{.7
  (format nil "~1{~
~}" nil)
  "")

(deftest format.{.8
  (loop for i from 0 to 10
	for s = (format nil "~v{~A~}" i '(1 2 3 4 5 6 7 8 9 0))
	unless (string= s (subseq "1234567890" 0 i))
	collect (list i s))
  nil)

(deftest format.{.9
  (format nil "~#{~A~}" '(1 2 3 4 5 6 7) nil nil nil)
  "1234")

;;; ~:{ ... ~}

(deftest format.\:{.1
  (format nil "~:{(~A ~A)~}" '((1 2 3)(4 5)(6 7 8)))
  "(1 2)(4 5)(6 7)")

;;; Tests of ~^ inside ~{ ... ~}

(deftest format.{.10
  (format nil "~{X ~A~^ Y ~A~^ ~}" '(1 2 3 4 5))
  "X 1 Y 2 X 3 Y 4 X 5")

(deftest format.{.11
  (format nil "~{X ~A~^ Y ~A~^ ~}" '(1 2 3 4))
  "X 1 Y 2 X 3 Y 4")

(deftest format.{.12
  (format nil "~1{~A~^~A~}" '(1))
  "1")

(deftest format.{.13
  (format nil "~0{~A~^~A~}" '(1))
  "")

(deftest format.{.14
  (format nil "~1{~A~^~A~}" '(1 2 3))
  "12")

(deftest format.{.15
  (format nil "~0{~}" "~A" '(1 2 3))
  "")

(deftest format.{.16
  (format nil "~1{~}" "~A" '(4 5 6))
  "4")




;;; Tests of ~@{ ... ~}

(deftest format.@{.1
  (format nil "~@{~
~}")
  "")

(deftest format.@{.1A
  (format nil "~@{~}" "")
  "")

(deftest format.@{.2
  (format nil "~@{ ~}")
  "")

(deftest format.@{.3
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (format nil "~@{X ~A Y Z~}" nil)))
  "X NIL Y Z")

(deftest format.@{.4
  (format nil "~@{~A~}" 1 2 3 4)
  "1234")

(deftest format.@{.5
  (format nil "~@{~{~A~}~}" '(1 2 3) '(4 5) '(6 7 8))
  "12345678")

(deftest format.@{.6
  (format nil "~@{~1{~A~}~}" '(1 2 3) '(4 5) '(6 7 8))
  "146")

(deftest format.@{.7
  (format nil "~1@{~ FOO ~}")
  "")







  

