;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Apr 20 22:36:53 2004
;;;; Contains: Tests of vector printing

(in-package :cl-test)

;;; Empty vector tests

(deftest print.vector.1
  (with-standard-io-syntax
   (write-to-string #() :readably nil :array t))
  "#()")

(deftest print.vector.2
  (with-standard-io-syntax
   (loop for i from 2 to 100
	 for a = (make-array '(0) :element-type `(unsigned-byte ,i))
	 for s = (write-to-string a :readably nil :array t :pretty nil)
	 unless (string= s "#()")
	 collect (list i s)))
  nil)

(deftest print.vector.3
  (with-standard-io-syntax
   (loop for i from 1 to 100
	 for a = (make-array '(0) :element-type `(signed-byte ,i))
	 for s = (write-to-string a :readably nil :array t :pretty nil)
	 unless (string= s "#()")
	 collect (list i s)))
  nil)

(deftest print.vector.4
  (with-standard-io-syntax
   (loop for type in '(short-float single-float double-float long-float)
	 for a = (make-array '(0) :element-type type)
	 for s = (write-to-string a :readably nil :array t :pretty nil)
	 unless (string= s "#()")
	 collect (list type s)))
  nil)

;;; Nonempty vectors

(deftest print.vector.5
  (with-standard-io-syntax
   (let* ((*package* (find-package "CL-TEST"))
	  (result
	   (write-to-string #(a b c)
			    :readably nil :array t
			    :pretty nil :case :downcase)))
     (or (and (string= result "#(a b c)") t)
	 result)))
  t)

(deftest print.vector.6
  (with-standard-io-syntax
   (loop
    for i from 2 to 100
    for a = (make-array '(4) :element-type `(unsigned-byte ,i)
			:initial-contents '(3 0 2 1))
    for s = (write-to-string a :readably nil :array t :pretty nil)
    unless (string= s "#(3 0 2 1)")
    collect (list i a s)))
  nil)

(deftest print.vector.7
  (with-standard-io-syntax
   (loop
    for i from 2 to 100
    for a = (make-array '(4) :element-type `(signed-byte ,i)
			:initial-contents '(-2 -1 0 1))
    for s = (write-to-string a :readably nil :array t :pretty nil)
    unless (string= s "#(-2 -1 0 1)")
    collect (list i a s)))
  nil)


