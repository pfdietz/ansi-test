;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Oct 17 21:32:45 2004
;;;; Contains: Tests of the ~P format directives

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(deftest format.p.1
  (format nil "~p" 1)
  "")

(deftest format.p.2
  (format nil "~P" 2)
  "s")

(deftest format.p.3
  (format nil "~p" 0)
  "s")

(deftest format.p.4
  (format nil "~P" 1.0)
  "s")

(deftest format.p.5
  (loop for x in *universe*
	for s = (format nil "~p" x)
	unless (or (eql x 1) (string= s "s"))
	collect (list x s))
  nil)

;;; :p

(deftest format.p.6
  (format nil "~D cat~:P" 1)
  "1 cat")

(deftest format.p.7
  (format nil "~D cat~:p" 2)
  "2 cats")

(deftest format.p.8
  (format nil "~D cat~:P" 0)
  "0 cats")

(deftest format.p.9
  (format nil "~D cat~:p" "No")
  "No cats")

;;; :@p

(deftest format.p.10
  (format nil "~D penn~:@P" 1)
  "1 penny")

(deftest format.p.11
  (format nil "~D penn~:@p" 2)
  "2 pennies")

(deftest format.p.12
  (format nil "~D penn~@:P" 0)
  "0 pennies")

(deftest format.p.13
  (format nil "~D penn~@:p" "No")
  "No pennies")

;;; @p

(deftest format.p.14
  (format nil "~@p" 1)
  "y")

(deftest format.p.15
  (format nil "~@P" 2)
  "ies")

(deftest format.p.16
  (format nil "~@p" 0)
  "ies")

(deftest format.p.17
  (format nil "~@P" 1.0)
  "ies")

(deftest format.p.18
  (loop for x in *universe*
	for s = (format nil "~@p" x)
	unless (or (eql x 1) (string= s "ies"))
	collect (list x s))
  nil)
