;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Apr 22 22:38:11 2004
;;;; Contains: Tests of printing of arrays (other than vectors)

(compile-and-load "printer-aux.lsp")

(in-package :cl-test)

;; Zero dimensional arrays

(deftest print.array.0.1
  (let ((a (make-array nil :initial-element 0)))
    (with-standard-io-syntax
     (write-to-string a :readably nil :array t)))
  "#0A0")

(deftest print.array.0.2
  (with-standard-io-syntax
   (let ((a (make-array nil :initial-element '|A|))
	 (*package* (find-package "CL-TEST")))
     (write-to-string a :readably nil :array t)))
  "#0AA")

(deftest print.array.0.3
  (let ((a (make-array nil :initial-element 0)))
    (subseq (write-to-string a :readably nil :array nil) 0 2))
  "#<")

(deftest print.array.0.4
   (let ((a (make-array nil :initial-element 0 :adjustable t)))
    (with-standard-io-syntax
     (write-to-string a :readably nil :array t)))
  "#0A0")

(deftest print.array.0.5
   (let* ((a (make-array nil :initial-element 0 :adjustable t))
	  (b (make-array nil :displaced-to a :displaced-index-offset 0)))
    (with-standard-io-syntax
     (write-to-string b :readably nil :array t)))
  "#0A0")

(deftest print.array.0.6
  (let ((a (make-array nil :initial-element 0
		       :element-type '(integer 0 2))))
    (with-standard-io-syntax
     (write-to-string a :readably nil :array t)))
  "#0A0")

;; Two-d arrays
(deftest print.array.2.1
  (let ((a (make-array '(1 1) :initial-contents '((1)))))
    (with-standard-io-syntax
     (write-to-string a :readably nil :array t)))
  "#2A((1))")

(deftest print.array.2.2
  (let ((a (make-array '(2 3) :initial-contents '((1 3 8)(2 6 10)))))
    (with-standard-io-syntax
     (write-to-string a :readably nil :array t)))
  "#2A((1 3 8) (2 6 10))")

(deftest print.array.2.3
  (let ((a (make-array '(0 1))))
    (with-standard-io-syntax
     (write-to-string a :readably nil :array t)))
  "#2A()")

(deftest print.array.2.4
  (let ((a (make-array '(1 0))))
    (with-standard-io-syntax
     (write-to-string a :readably nil :array t)))
  "#2A(())")

(deftest print.array.2.5
  (let ((a (make-array '(0 0))))
    (with-standard-io-syntax
     (write-to-string a :readably nil :array t)))
  "#2A()")






