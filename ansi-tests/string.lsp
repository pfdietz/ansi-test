;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Sep 30 19:16:59 2002
;;;; Contains: Tests for string related functions and classes

(in-package :cl-test)

(deftest string.1
  (subtypep* 'string 'array)
  t t)

(deftest string.2
  (subtypep* 'string 'vector)
  t t)

(deftest string.3
  (subtypep* 'string 'sequence)
  t t)

(deftest string.4
  (let ((s (string #\a)))
    (values (notnot (stringp s)) s))
  t "a")

(deftest string.5
  (let ((s (string "")))
    (values (notnot (stringp s)) s))
  t "")

(deftest string.6
  (let ((s (string '|FOO|)))
    (values (notnot (stringp s)) s))
  t "FOO")

(deftest string.7
  (loop for x in *universe*
	always (handler-case (stringp (string x))
			     (type-error () :caught)))
  t)

;;; Tests of base-string

(deftest base-string.1
  (subtypep* 'base-string 'string)
  t t)

(deftest base-string.2
  (subtypep* 'base-string 'vector)
  t t)

(deftest base-string.3
  (subtypep* 'base-string 'array)
  t t)

(deftest base-string.4
  (subtypep* 'base-string 'sequence)
  t t)

;;; Tests of simple-string

(deftest simple-string.1
  (subtypep* 'simple-string 'string)
  t t)

(deftest simple-string.2
  (subtypep* 'simple-string 'vector)
  t t)

(deftest simple-string.3
  (subtypep* 'simple-string 'simple-array)
  t t)

(deftest simple-string.4
  (subtypep* 'simple-string 'array)
  t t)

(deftest simple-string.5
  (subtypep* 'simple-string 'sequence)
  t t)

;;; Tests for simple-base-string

(deftest simple-base-string.1
  (subtypep* 'simple-base-string 'string)
  t t)

(deftest simple-base-string.2
  (subtypep* 'simple-base-string 'vector)
  t t)

(deftest simple-base-string.3
  (subtypep* 'simple-base-string 'simple-array)
  t t)

(deftest simple-base-string.4
  (subtypep* 'simple-base-string 'array)
  t t)

(deftest simple-base-string.5
  (subtypep* 'simple-base-string 'sequence)
  t t)

(deftest simple-base-string.6
  (subtypep* 'simple-base-string 'base-string)
  t t)

(deftest simple-base-string.7
  (subtypep* 'simple-base-string 'simple-string)
  t t)

(deftest simple-base-string.8
  (subtypep* 'simple-base-string 'simple-vector)
  nil t)

;;; Tests for simple-string-p

(deftest simple-string-p.1
  (loop for x in *universe*
	always (if (typep x 'simple-string)
		   (simple-string-p x)
		 (not (simple-string-p x))))
  t)

(deftest simple-string-p.2
  (notnot (simple-string-p "ancd"))
  t)

(deftest simple-string-p.3
  (simple-string-p 0)
  nil)

(deftest simple-string-p.4
  (simple-string-p (make-array 4 :element-type 'character
			       :initial-contents '(#\a #\a #\a #\b)
			       :fill-pointer t))
  nil)

(deftest simple-string-p.5
  (notnot (simple-string-p (make-array
			    4 :element-type 'base-char
			    :initial-contents '(#\a #\a #\a #\b))))
  t)

(deftest simple-string-p.6
  (notnot (simple-string-p (make-array
			    4 :element-type 'standard-char
			    :initial-contents '(#\a #\a #\a #\b))))
  t)

(deftest simple-string-p.7
  (let* ((s (make-array 10 :element-type 'character
			:initial-element #\a))
	 (s2 (make-array 4 :element-type 'character
			 :displaced-to s
			 :displaced-index-offset 2)))
    (simple-string-p s2))
  nil)

;;; Tests of stringp

(deftest stringp.1
  (loop for x in *universe*
	always (if (typep x 'string)
		   (stringp x)
		 (not (stringp x))))
  t)

(deftest stringp.2
  (notnot (stringp "abcd"))
  t)

(deftest stringp.3
  (notnot (stringp (make-array 4 :element-type 'character
			       :initial-contents '(#\a #\b #\c #\d))))
  t)

(deftest stringp.4
  (notnot (stringp (make-array 4 :element-type 'base-char
			       :initial-contents '(#\a #\b #\c #\d))))
  t)

(deftest stringp.5
  (notnot (stringp (make-array 4 :element-type 'standard-char
			       :initial-contents '(#\a #\b #\c #\d))))
  t)

(deftest stringp.6
  (stringp 0)
  nil)

(deftest stringp.7
  (stringp #\a)
  nil)

(deftest stringp.8
  (let* ((s (make-array 10 :element-type 'character
			:initial-element #\a))
	 (s2 (make-array 4 :element-type 'character
			 :displaced-to s
			 :displaced-index-offset 2)))
    (notnot (stringp s2)))
  t)
