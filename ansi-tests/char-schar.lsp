;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Sep 29 21:04:44 2002
;;;; Contains: Tests of CHAR and SCHAR accessors

(in-package :cl-test)

(deftest char.1
  (let ((s "abcd"))
    (values (char s 0) (char s 1) (char s 2) (char s 3)))
  #\a #\b #\c #\d)

(deftest char.2
  (let ((s0 (copy-seq "abcd"))
	(s1 (copy-seq "abcd"))
	(s2 (copy-seq "abcd"))
	(s3 (copy-seq "abcd")))
    (setf (char s0 0) #\X)
    (setf (char s1 1) #\X)
    (setf (char s2 2) #\X)
    (setf (char s3 3) #\X)
    (values s0 s1 s2 s3))
  "Xbcd" "aXcd" "abXd" "abcX")

(deftest char.3
  (let ((s (make-array 6 :element-type 'character
		       :initial-contents '(#\a #\b #\c #\d #\e #\f))))
    (setf (char s 3) #\X)
    s)
  "abcXef")

(deftest char.4
  (let ((s (make-array 6 :element-type 'character
		       :initial-contents '(#\a #\b #\c #\d #\e #\f)
		       :fill-pointer 4)))
    (setf (char s 3) #\X)
    s)
  "abcX")

(deftest char.5
  (let ((s (make-string 5 :initial-element #\a)))
    (setf (char s 3) #\X)
    s)
  "aaaXa")

(deftest char.6
  (let ((s (make-string 5 :initial-element #\a :element-type 'base-char)))
    (setf (char s 3) #\X)
    s)
  "aaaXa")

(deftest char.7
  (let ((s (make-string 5 :initial-element #\a :element-type 'character)))
    (setf (char s 3) #\X)
    s)
  "aaaXa")

(deftest char.8
  (let ((s (make-array 6 :element-type 'character
		       :initial-contents '(#\a #\b #\c #\d #\e #\f)
		       :fill-pointer 4)))
    (setf (char s 5) #\X)
    (setf (fill-pointer s) 6)
    s)
  "abcdeX")

(deftest char.9
  (let ((s (make-string 5 :initial-element #\a
			:element-type 'base-char)))
    (setf (char s 3) #\X)
    s)
  "aaaXa")

(deftest char.10
  (let ((s (make-string 5 :initial-element #\a
			:element-type 'standard-char)))
    (setf (char s 3) #\X)
    s)
  "aaaXa")

;;; Tests of schar

(deftest schar.1
  (let ((s "abcd")) (values (schar s 0) (schar s 1) (schar s 2) (schar s 3)))
  #\a #\b #\c #\d)

(deftest schar.2
  (let ((s0 (copy-seq "abcd"))
	(s1 (copy-seq "abcd"))
	(s2 (copy-seq "abcd"))
	(s3 (copy-seq "abcd")))
    (setf (schar s0 0) #\X)
    (setf (schar s1 1) #\X)
    (setf (schar s2 2) #\X)
    (setf (schar s3 3) #\X)
    (values s0 s1 s2 s3))
  "Xbcd" "aXcd" "abXd" "abcX")

(deftest schar.3
  (let ((s (make-string 6 :initial-element #\x)))
    (setf (schar s 2) #\X)
    s)
  "xxXxxx")

(deftest schar.4
  (let ((s (make-string 6 :initial-element #\x :element-type 'character)))
    (setf (schar s 2) #\X)
    s)
  "xxXxxx")

(deftest schar.5
  (let ((s (make-string 6 :initial-element #\x :element-type 'standard-char)))
    (setf (schar s 2) #\X)
    s)
  "xxXxxx")

(deftest schar.6
  (let ((s (make-string 6 :initial-element #\x :element-type 'base-char)))
    (setf (schar s 2) #\X)
    s)
  "xxXxxx")

(deftest schar.7
  (let ((s (make-string 6 :initial-element #\x
			:element-type 'standard-char)))
    (setf (schar s 2) #\X)
    s)
  "xxXxxx")




