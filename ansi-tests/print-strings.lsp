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
  (loop for c across +standard-chars+
	for s1 = (string c)
	for s2 = (write-to-string s1 :escape nil :readably nil)
	unless (string= s1 s2)
	collect (list c s1 s2))
  nil)

(deftest print.string.3
  (loop for i below 256
	for c = (code-char i)
	when c
	nconc
	(let* ((s1 (string c))
	       (s2 (write-to-string s1 :escape nil :readably nil)))
	  (unless (string= s1 s2)
	    (list (list c s1 s2)))))
  nil)

(deftest print.string.4
  (loop for c across +standard-chars+
	for s1 = (string c)
	for s2 = (write-to-string s1 :escape t :readably nil)
	unless (or (find c "\"\\") (string= (concatenate 'string "\"" s1 "\"") s2))
	collect (list c s1 s2))
  nil)

(deftest print.string.5
  (write-to-string "\"" :escape t :readably nil)
  "\"\\\"\"")

(deftest print.string.6
  (write-to-string "\\" :escape t :readably nil)
  "\"\\\\\"")



