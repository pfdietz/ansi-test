;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Nov 11 20:17:51 2004
;;;; Contains: Tests of the ~^ format directive (inside other format constructs)

(in-package :cl-test)
(compile-and-load "printer-aux.lsp")

;;; Tests of ~^ inside ~{ ... ~}

(deftest format.^.{.1
  (format nil "~{X ~A~^ Y ~A~^ ~}" '(1 2 3 4 5))
  "X 1 Y 2 X 3 Y 4 X 5")

(deftest format.^.{.2
  (format nil "~{X ~A~^ Y ~A~^ ~}" '(1 2 3 4))
  "X 1 Y 2 X 3 Y 4")

(deftest format.^.{.3
  (format nil "~1{~A~^~A~}" '(1))
  "1")

(deftest format.^.{.4
  (format nil "~0{~A~^~A~}" '(1))
  "")

(deftest format.^.{.5
  (format nil "~1{~A~^~A~}" '(1 2 3))
  "12")

(deftest format.^.{.6
  (format nil "~{~A~A~0^~A~}" '(1 2 3 4 5 6))
  "12")

(deftest format.^.{.7
  (format nil "~{~A~A~v^~A~}" '(1 2 3 4 5 6 0 7 8 9 10 11 12))
  "12456")

(deftest format.^.{.8
  (format nil "~{~#,3^~A~}" '(1 2 3 4 5 6 7 8 9 10))
  "1234567")

(deftest format.^.{.9
  (format nil "~{~2,#^~A~}~A" '(1 2 3 4 5 6 7 8 9 10) 0)
  "123456780")

(deftest format.^.{.10
  (format nil "~{~#,#^~A~}" '(1 2 3 4 5 6 7 8 9 10))
  "")

(deftest format.^.{.11
  (format nil "~{~#,#,#^~A~}" '(1 2 3 4 5 6 7 8 9 10))
  "")

(deftest format.^.{.12
  (format nil "~{~#,1,2^~A~}" '(1 2 3 4 5 6 7 8 9 10))
  "123456789")

(deftest format.^.{.13
  (format nil "~{~#,#,v^~A~}" '(1 2 3 4 5 6 7 8 9 10))
  "246")

(deftest format.^.{.14
  (format nil "~{~#,#,v^~A~}" '(1 2 3 4 5 6 7 8 9 10 11))
  "246")

(deftest format.^.{.15
  (format nil "~{~#,#,v^~A~}" '(1 2 3 4 5 6 7 8 9 10 11 12))
  "246")

(deftest format.^.{.16
  (format nil "~{~#,#,v^~A~}" '(1 2 3 4 5 6 7 8 9 10 11 12 13))
  "246")

(deftest format.^.{.17
  (format nil "~{~#,#,v^~A~}" '(1 2 3 4 5 6 7 8 9 10 11 12 13 14))
  "2468")

