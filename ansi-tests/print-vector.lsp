;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Apr 20 22:36:53 2004
;;;; Contains: Tests of vector printing

(in-package :cl-test)

(deftest print.vector.1
  (with-standard-io-syntax
   (write-to-string #() :readably nil :array t))
  "#()")

(deftest print.vector.2
  (with-standard-io-syntax
   (loop for i from 2 to 100
	 for a = (make-array '(0) :element-type `(unsigned-byte ,i))
	 for s = (write-to-string a :readably nil :array t)
	 unless (string= s "#()")
	 collect (list i s)))
  nil)

(deftest print.vector.3
  (with-standard-io-syntax
   (loop for i from 1 to 100
	 for a = (make-array '(0) :element-type `(signed-byte ,i))
	 for s = (write-to-string a :readably nil :array t)
	 unless (string= s "#()")
	 collect (list i s)))
  nil)

(deftest print.vector.4
  (with-standard-io-syntax
   (loop for type in '(short-float single-float double-float long-float)
	 for a = (make-array '(0) :element-type type)
	 for s = (write-to-string a :readably nil :array t)
	 unless (string= s "#()")
	 collect (list type s)))
  nil)
