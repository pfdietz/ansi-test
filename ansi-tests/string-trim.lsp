;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Oct  3 21:53:38 2002
;;;; Contains: Tests for STRING-TRIM

(in-package :cl-test)

(deftest string-trim.1
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "cd")

(deftest string-trim.2
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-trim '(#\a #\b) s)))
    (values s s2))
  "abcdaba"
  "cd")

(deftest string-trim.3
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-trim #(#\a #\b) s)))
    (values s s2))
  "abcdaba"
  "cd")

(deftest string-trim.4
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-trim (make-array 2 :initial-contents '(#\a #\b))
			  s)))
    (values s s2))
  "abcdaba"
  "cd")

(deftest string-trim.5
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-trim (make-array 2 :initial-contents '(#\a #\b)
				      :element-type 'character)
			  s)))
    (values s s2))
  "abcdaba"
  "cd")

(deftest string-trim.6
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-trim (make-array 2 :initial-contents '(#\a #\b)
				      :element-type 'standard-char)
			  s)))
    (values s s2))
  "abcdaba"
  "cd")

(deftest string-trim.7
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-trim (make-array 2 :initial-contents '(#\a #\b)
				      :element-type 'base-char)
			  s)))
    (values s s2))
  "abcdaba"
  "cd")

(deftest string-trim.8
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-trim (make-array 2 :initial-contents '(#\a #\b #\c #\d)
				      :element-type 'character
				      :fill-pointer 2)
			  s)))
    (values s s2))
  "abcdaba"
  "cd")

(deftest string-trim.9
  (let* ((s (make-array 7 :initial-contents (coerce "abcdaba" 'list)
			:element-type 'character
			))
	 (s2 (string-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "cd")

(deftest string-trim.10
  (let* ((s (make-array 9 :initial-contents (coerce "abcdabadd" 'list)
			:element-type 'character
			:fill-pointer 7))
	 (s2 (string-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "cd")

(deftest string-trim.11
  (let* ((s (make-array 7 :initial-contents (coerce "abcdaba" 'list)
			:element-type 'standard-char
			))
	 (s2 (string-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "cd")

(deftest string-trim.12
  (let* ((s (make-array 7 :initial-contents (coerce "abcdaba" 'list)
			:element-type 'base-char
			))
	 (s2 (string-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "cd")

