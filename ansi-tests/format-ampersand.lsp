;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jul 27 23:52:20 2004
;;;; Contains: Tests of format with ~& directive

(in-package :cl-test)

(deftest format.&.1
  (format nil "~0&")
  "")

(deftest format.&.2
  (format nil "~&")
  "")

(deftest format.&.3
  (format nil "X~&")
  "X
")

(deftest format.&.4
  (format nil "X~%~&")
  "X
")

(deftest format.&.5
  (loop for i from 1 to 100
	for s1 = (make-string (1- i) :initial-element #\Newline)
	for s2 = (format nil (format nil "~~~D&" i))
	unless (string= s1 s2)
	collect i)
  nil)

(deftest format.&.6
  (loop for i from 1 to 100
	for s1 = (concatenate 'string
			      "X"
			      (make-string i :initial-element #\Newline))
	for s2 = (format nil (format nil "X~~~D&" i))
	unless (string= s1 s2)
	collect i)
  nil)

(deftest format.&.7
  (format nil "~v&" nil)
  "")

(deftest format.&.8
  (format nil "X~v&" nil)
  "X
")

(deftest format.&.9
  (loop for i from 1 to 100
	for s1 = (make-string (1- i) :initial-element #\Newline)
	for s2 = (format nil "~V&" i)
	unless (string= s1 s2)
	collect i)
  nil)

(deftest format.&.10
  (loop for i from 1 to (min (- call-arguments-limit 3) 100)
	for s1 = (make-string (1- i) :initial-element #\Newline)
	for args = (make-list i)
	for s2 = (apply #'format nil "~#&" args)
	unless (string= s1 s2)
	collect i)
  nil)

(deftest format.&.11
  (format nil "X~V%" 0)
  "X")

(deftest format.&.12
  (format nil "X~#%")
  "X")

(deftest format.&.13
  (format nil "X~#%" 'a 'b 'c)
  "X


")











