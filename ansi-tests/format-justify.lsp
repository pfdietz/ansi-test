;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Aug 22 18:09:49 2004
;;;; Contains: Tests of the ~< ~> directive

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(def-pprint-test format.justify.1
  (format nil "~<~>")
  "")

(def-pprint-test format.justify.2
  (loop for i from 1 to 20
	for s1 = (make-string i :initial-element #\x)
	for s2 = (format nil "~<~A~>" s1)
	unless (string= s1 s2)
	collect (list i s1 s2))
  nil)

(def-pprint-test format.justify.3
  (loop for i from 1 to 20
	for s1 = (make-string i :initial-element #\x)
	for s2 = (format nil "~<~A~;~A~>" s1 s1)
	unless (string= s2 (concatenate 'string s1 s1))
	collect (list i s1 s2))
  nil)

(def-pprint-test format.justify.4
  (loop for i from 1 to 20
	for s1 = (make-string i :initial-element #\x)
	for expected = (concatenate 'string s1 " " s1)
	for s2 = (format nil "~,,1<~A~;~A~>" s1 s1)
	unless (string= s2 expected)
	collect (list i expected s2))
  nil)

(def-pprint-test format.justify.5
  (loop for i from 1 to 20
	for s1 = (make-string i :initial-element #\x)
	for expected = (concatenate 'string s1 "," s1)
	for s2 = (format nil "~,,1,',<~A~;~A~>" s1 s1)
	unless (string= s2 expected)
	collect (list i expected s2))
  nil)

(def-pprint-test format.justify.6
  (loop for i from 1 to 20
	for s1 = (make-string i :initial-element #\x)
	for expected = (concatenate 'string s1 "  " s1)
	for s2 = (format nil "~,,2<~A~;~A~>" s1 s1)
	unless (string= s2 expected)
	collect (list i expected s2))
  nil)

