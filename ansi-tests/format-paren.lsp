;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Oct 17 20:28:24 2004
;;;; Contains: Tests of the ~( format directives

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(deftest format.paren.1
  (format nil "~(XXyy~AuuVV~)" "ABc dEF ghI")
  "xxyyabc def ghiuuvv")

;;; Conversion of simple characters to downcase
(deftest format.paren.2
  (loop for i from 0 below (min char-code-limit (ash 1 16))
	for c = (code-char i)
	when (and c
		  (eql (char-code c) (char-int c))
		  (upper-case-p c)
		  (let ((s1 (format nil "~(~c~)" c))
			(s2 (string (char-downcase c))))
		    (if
			(or (not (eql (length s1) 1))
			    (not (eql (length s2) 1))
			    (not (eql (elt s1 0)
				      (elt s2 0))))
			(list i c s1 s2)
		      nil)))
	collect it)
  nil)

(deftest format.paren.3
  (format nil "~@(this is a TEST.~)")
  "This is a test.")

(deftest format.paren.4
  (format nil "~@(!@#$%^&*this is a TEST.~)")
  "!@#$%^&*This is a test.")

(deftest format.paren.5
  (format nil "~:(this is a TEST.~)")
  "This Is A Test.")
			
(deftest format.paren.6
  (format nil "~:(this is7a TEST.~)")
  "This Is7a Test.")

(deftest format.paren.7
  (format nil "~:@(this is AlSo A teSt~)")
  "THIS IS ALSO A TEST")

(deftest format.paren.8
  (loop for i from 0 below (min char-code-limit (ash 1 16))
	for c = (code-char i)
	when (and c
		  (eql (char-code c) (char-int c))
		  (lower-case-p c)
		  (let ((s1 (format nil "~@:(~c~)" c))
			(s2 (string (char-upcase c))))
		    (if
			(or (not (eql (length s1) 1))
			    (not (eql (length s2) 1))
			    (not (eql (elt s1 0)
				      (elt s2 0))))
			(list i c s1 s2)
		      nil)))
	collect it)
  nil)

;;; Nested conversion

(deftest format.paren.9
  (format nil "~(aBc ~:(def~) GHi~)")
  "abc def ghi")

(deftest format.paren.10
  (format nil "~(aBc ~(def~) GHi~)")
  "abc def ghi")

(deftest format.paren.11
  (format nil "~@(aBc ~:(def~) GHi~)")
  "Abc def ghi")

(deftest format.paren.12
  (format nil "~(aBc ~@(def~) GHi~)")
  "abc def ghi")

(deftest format.paren.13
  (format nil "~(aBc ~:(def~) GHi~)")
  "abc def ghi")

(deftest format.paren.14
  (format nil "~:(aBc ~(def~) GHi~)")
  "Abc Def Ghi")

(deftest format.paren.15
  (format nil "~:(aBc ~:(def~) GHi~)")
  "Abc Def Ghi")

(deftest format.paren.16
  (format nil "~:(aBc ~@(def~) GHi~)")
  "Abc Def Ghi")

(deftest format.paren.17
  (format nil "~:(aBc ~@:(def~) GHi~)")
  "Abc Def Ghi")

(deftest format.paren.18
  (format nil "~@(aBc ~(def~) GHi~)")
  "Abc def ghi")

(deftest format.paren.19
  (format nil "~@(aBc ~:(def~) GHi~)")
  "Abc def ghi")

(deftest format.paren.20
  (format nil "~@(aBc ~@(def~) GHi~)")
  "Abc def ghi")

(deftest format.paren.21
  (format nil "~@(aBc ~@:(def~) GHi~)")
  "Abc def ghi")

(deftest format.paren.22
  (format nil "~:@(aBc ~(def~) GHi~)")
  "ABC DEF GHI")

(deftest format.paren.23
  (format nil "~@:(aBc ~:(def~) GHi~)")
  "ABC DEF GHI")

(deftest format.paren.24
  (format nil "~:@(aBc ~@(def~) GHi~)")
  "ABC DEF GHI")

(deftest format.paren.25
  (format nil "~@:(aBc ~@:(def~) GHi~)")
  "ABC DEF GHI")
