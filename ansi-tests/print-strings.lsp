;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Apr 19 05:53:48 2004
;;;; Contains: Tests of string printing

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(deftest print.string.1
  (with-standard-io-syntax
   (write-to-string "" :escape nil :readably nil))
  "")

(deftest print.string.2
  (with-standard-io-syntax
   (loop for c across +standard-chars+
	 for s1 = (string c)
	 for s2 = (write-to-string s1 :escape nil :readably nil)
	 unless (string= s1 s2)
	 collect (list c s1 s2)))
  nil)

(deftest print.string.3
  (with-standard-io-syntax
   (loop for i below 256
	 for c = (code-char i)
	 when c
	 nconc
	 (let* ((s1 (string c))
		(s2 (write-to-string s1 :escape nil :readably nil)))
	   (unless (string= s1 s2)
	     (list (list c s1 s2))))))
  nil)

(deftest print.string.4
  (with-standard-io-syntax
   (loop for c across +standard-chars+
	 for s1 = (string c)
	 for s2 = (write-to-string s1 :escape t :readably nil)
	 unless (or (find c "\"\\") (string= (concatenate 'string "\"" s1 "\"") s2))
	 collect (list c s1 s2)))
  nil)

(deftest print.string.5
  (with-standard-io-syntax
   (write-to-string "\"" :escape t :readably nil))
  "\"\\\"\"")

(deftest print.string.6
  (with-standard-io-syntax
   (write-to-string "\\" :escape t :readably nil))
  "\"\\\\\"")

;;; Not affected by *print-array*

(deftest print.string.7
  (with-standard-io-syntax
   (loop for s1 in (remove-if-not #'stringp *universe*)
	 for s2 = (write-to-string s1 :escape nil :readably nil)
	 for s3 = (write-to-string s1 :array t :escape nil :readably nil)
	 unless (string= s2 s3)
	 collect (list s1 s2 s3)))
  nil)

(deftest print.string.8
  (with-standard-io-syntax
   (loop for s1 in (remove-if-not #'stringp *universe*)
	 for s2 = (write-to-string s1 :escape t :readably nil)
	 for s3 = (write-to-string s1 :array t :escape t :readably nil)
	 unless (string= s2 s3)
	 collect (list s1 s2 s3)))
  nil)

;;; Only active elements of the string are printed

(deftest print.string.9
  (let ((s (make-array '(10) :fill-pointer 5 :element-type 'character
		       :initial-contents "abcdefghij")))
    (with-standard-io-syntax
     (write-to-string s :escape nil :readably nil)))
  "abcde")

(deftest print.string.10
  (let ((s (make-array '(10) :fill-pointer 5 :element-type 'character
		       :initial-contents "aBcDefGHij")))
    (with-standard-io-syntax
     (write-to-string s :escape t :readably nil)))
  "\"aBcDe\"")

;;; Random tests

(deftest print.string.random.1
  (loop for len = (1+ (random 5))
	for s = (coerce (loop repeat len collect (random-from-seq +standard-chars+))
			'string)
	repeat 1000
	append (randomly-check-readability s))
  nil)


