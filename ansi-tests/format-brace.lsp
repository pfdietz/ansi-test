;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Aug 25 22:08:51 2004
;;;; Contains: Tests of the ~{ ... ~} format directives

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(deftest format.{.1
  (format nil "~{~
~}" nil)
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
