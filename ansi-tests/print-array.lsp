;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Apr 22 22:38:11 2004
;;;; Contains: Tests of printing of arrays (other than vectors)

(compile-and-load "printer-aux.lsp")

(in-package :cl-test)

;; Zero dimensional arrays

(deftest print.array.1
  (let ((a (make-array nil :initial-element 0)))
    (with-standard-io-syntax
     (write-to-string a :readably nil :array t)))
  "#0A0")

(deftest print.array.2
  (with-standard-io-syntax
   (let ((a (make-array nil :initial-element '|A|))
	 (*package* (find-package "CL-TEST")))
     (write-to-string a :readably nil :array t)))
  "#0AA")

(deftest print.array.3
  (let ((a (make-array nil :initial-element 0)))
    (subseq (write-to-string a :readably nil :array nil) 0 2))
  "#<")


