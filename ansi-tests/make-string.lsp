;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Oct  5 12:32:20 2002
;;;; Contains: Tests for MAKE-STRING

(in-package :cl-test)

(deftest make-string.1
  (let ((s (make-string 10)))
    (and (stringp s)
	 ;; (string-all-the-same s)
	 (eqlt (length s) 10)
	 ))
  t)

(deftest make-string.2
  (let ((s (make-string 10 :initial-element #\a)))
    (and (stringp s)
	 (eql (length s) 10)
	 s))
  "aaaaaaaaaa")

(deftest make-string.3
  (let ((s (make-string 10 :initial-element #\a
			:element-type 'character)))
    (and (stringp s)
	 (eql (length s) 10)
	 s))
  "aaaaaaaaaa")

(deftest make-string.4
  (let ((s (make-string 10 :initial-element #\a
			:element-type 'standard-char)))
    (and (stringp s)
	 (eql (length s) 10)
	 s))
  "aaaaaaaaaa")

(deftest make-string.5
  (let ((s (make-string 10 :initial-element #\a
			:element-type 'base-char)))
    (and (stringp s)
	 (eql (length s) 10)
	 s))
  "aaaaaaaaaa")

(deftest make-string.6
  (make-string 0)
  "")

(deftest make-string.7
  (let ((s (make-string 10 :element-type 'character)))
    (and (stringp s)
	 (eqlt (length s) 10)
	 #| (string-all-the-same s) |#
	 ))
  t)

(deftest make-string.8
  (let ((s (make-string 10 :element-type 'standard-char)))
    (and (stringp s)
	 (eqlt (length s) 10)
	 #| (string-all-the-same s) |#
	 ))
  t)

(deftest make-string.9
  (let ((s (make-string 10 :element-type 'base-char)))
    (and (stringp s)
	 (eqlt (length s) 10)
	 #| (string-all-the-same s) |#
	 ))
  t)





