;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct  4 04:59:46 2002
;;;; Contains: Tests of STRING-RIGHT-TRIM

(in-package :cl-test)

(deftest string-right-trim.1
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-right-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "abcd")

(deftest string-right-trim.2
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-right-trim '(#\a #\b) s)))
    (values s s2))
  "abcdaba"
  "abcd")

(deftest string-right-trim.3
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-right-trim #(#\a #\b) s)))
    (values s s2))
  "abcdaba"
  "abcd")

(deftest string-right-trim.4
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-right-trim (make-array 2 :initial-contents '(#\a #\b))
			  s)))
    (values s s2))
  "abcdaba"
  "abcd")

(deftest string-right-trim.5
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-right-trim (make-array 2 :initial-contents '(#\a #\b)
				      :element-type 'character)
			  s)))
    (values s s2))
  "abcdaba"
  "abcd")

(deftest string-right-trim.6
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-right-trim (make-array 2 :initial-contents '(#\a #\b)
				      :element-type 'standard-char)
			  s)))
    (values s s2))
  "abcdaba"
  "abcd")

(deftest string-right-trim.7
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-right-trim (make-array 2 :initial-contents '(#\a #\b)
				      :element-type 'base-char)
			  s)))
    (values s s2))
  "abcdaba"
  "abcd")

(deftest string-right-trim.8
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-right-trim (make-array 2 :initial-contents '(#\a #\b #\c #\d)
				      :element-type 'character
				      :fill-pointer 2)
			  s)))
    (values s s2))
  "abcdaba"
  "abcd")

(deftest string-right-trim.9
  (let* ((s (make-array 7 :initial-contents (coerce "abcdaba" 'list)
			:element-type 'character
			))
	 (s2 (string-right-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "abcd")

(deftest string-right-trim.10
  (let* ((s (make-array 9 :initial-contents (coerce "abcdabadd" 'list)
			:element-type 'character
			:fill-pointer 7))
	 (s2 (string-right-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "abcd")

(deftest string-right-trim.11
  (let* ((s (make-array 7 :initial-contents (coerce "abcdaba" 'list)
			:element-type 'standard-char
			))
	 (s2 (string-right-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "abcd")

(deftest string-right-trim.12
  (let* ((s (make-array 7 :initial-contents (coerce "abcdaba" 'list)
			:element-type 'base-char
			))
	 (s2 (string-right-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "abcd")
