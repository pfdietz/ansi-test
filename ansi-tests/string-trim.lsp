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
	 (s2 (string-trim (make-array 4 :initial-contents '(#\a #\b #\c #\d)
				      :element-type 'character
				      :fill-pointer 2)
			  s)))
    (values s s2))
  "abcdaba"
  "cd")

(deftest string-trim.9
  (let* ((s (make-array 7 :initial-contents "abcdaba"
			:element-type 'character
			))
	 (s2 (string-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "cd")

(deftest string-trim.10
  (let* ((s (make-array 9 :initial-contents "abcdabadd"
			:element-type 'character
			:fill-pointer 7))
	 (s2 (string-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "cd")

(deftest string-trim.11
  (let* ((s (make-array 7 :initial-contents "abcdaba"
			:element-type 'standard-char
			))
	 (s2 (string-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "cd")

(deftest string-trim.12
  (let* ((s (make-array 7 :initial-contents "abcdaba"
			:element-type 'base-char
			))
	 (s2 (string-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "cd")

;;; Test that trimming is case sensitive
(deftest string-trim.13
  (let* ((s (copy-seq "Aa"))
	 (s2 (string-trim "a" s)))
    (values s s2))
  "Aa" "A")

(deftest string-trim.14
  (let* ((s '|abcdaba|)
	 (s2 (string-trim "ab" s)))
    (values (symbol-name s) s2))
  "abcdaba"
  "cd")

(deftest string-trim.15
  (string-trim "abc" "")
  "")

(deftest string-trim.16
  (string-trim "a" #\a)
  "")

(deftest string-trim.17
  (string-trim "b" #\a)
  "a")

(deftest string-trim.18
  (string-trim "" (copy-seq "abcde"))
  "abcde")

(deftest string-trim.19
  (string-trim "abc" (copy-seq "abcabcabc"))
  "")

(deftest string-trim.order.1
  (let ((i 0) x y)
    (values
     (string-trim (progn (setf x (incf i)) " ")
		  (progn (setf y (incf i))
			 (copy-seq "   abc d e f  ")))
     i x y))
  "abc d e f" 2 1 2)

;;; Error cases

(deftest string-trim.error.1
  (classify-error (string-trim))
  program-error)

(deftest string-trim.error.2
  (classify-error (string-trim "abc"))
  program-error)

(deftest string-trim.error.3
  (classify-error (string-trim "abc" "abcdddabc" nil))
  program-error)
