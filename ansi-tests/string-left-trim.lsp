;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct  4 04:57:41 2002
;;;; Contains: Tests for STRING-LEFT-TRIM

(in-package :cl-test)

(deftest string-left-trim.1
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-left-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "cdaba")

(deftest string-left-trim.2
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-left-trim '(#\a #\b) s)))
    (values s s2))
  "abcdaba"
  "cdaba")

(deftest string-left-trim.3
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-left-trim #(#\a #\b) s)))
    (values s s2))
  "abcdaba"
  "cdaba")

(deftest string-left-trim.4
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-left-trim (make-array 2 :initial-contents '(#\a #\b))
			  s)))
    (values s s2))
  "abcdaba"
  "cdaba")

(deftest string-left-trim.5
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-left-trim (make-array 2 :initial-contents '(#\a #\b)
				      :element-type 'character)
			  s)))
    (values s s2))
  "abcdaba"
  "cdaba")

(deftest string-left-trim.6
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-left-trim (make-array 2 :initial-contents '(#\a #\b)
				      :element-type 'standard-char)
			  s)))
    (values s s2))
  "abcdaba"
  "cdaba")

(deftest string-left-trim.7
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-left-trim (make-array 2 :initial-contents '(#\a #\b)
				      :element-type 'base-char)
			  s)))
    (values s s2))
  "abcdaba"
  "cdaba")

(deftest string-left-trim.8
  (let* ((s (copy-seq "abcdaba"))
	 (s2 (string-left-trim (make-array 4 :initial-contents '(#\a #\b #\c #\d)
				      :element-type 'character
				      :fill-pointer 2)
			  s)))
    (values s s2))
  "abcdaba"
  "cdaba")

(deftest string-left-trim.9
  (let* ((s (make-array 7 :initial-contents "abcdaba"
			:element-type 'character
			))
	 (s2 (string-left-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "cdaba")

(deftest string-left-trim.10
  (let* ((s (make-array 9 :initial-contents "abcdabadd"
			:element-type 'character
			:fill-pointer 7))
	 (s2 (string-left-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "cdaba")

(deftest string-left-trim.11
  (let* ((s (make-array 7 :initial-contents "abcdaba"
			:element-type 'standard-char
			))
	 (s2 (string-left-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "cdaba")

(deftest string-left-trim.12
  (let* ((s (make-array 7 :initial-contents "abcdaba"
			:element-type 'base-char
			))
	 (s2 (string-left-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "cdaba")

;;; Test that trimming is case sensitive
(deftest string-left-trim.13
  (let* ((s (copy-seq "aA"))
	 (s2 (string-left-trim "a" s)))
    (values s s2))
  "aA" "A")

(deftest string-left-trim.14
  (let* ((s '|abcdaba|)
	 (s2 (string-left-trim "ab" s)))
    (values (symbol-name s) s2))
  "abcdaba"
  "cdaba")

(deftest string-left-trim.15
  (string-left-trim "abc" "")
  "")

(deftest string-left-trim.16
  (string-left-trim "a" #\a)
  "")

(deftest string-left-trim.17
  (string-left-trim "b" #\a)
  "a")

(deftest string-left-trim.18
  (string-left-trim "" (copy-seq "abcde"))
  "abcde")

(deftest string-left-trim.19
  (string-left-trim "abc" (copy-seq "abcabcabc"))
  "")

(deftest string-left-trim.order.1
  (let ((i 0) x y)
    (values
     (string-left-trim (progn (setf x (incf i)) " ")
		       (progn (setf y (incf i))
			      (copy-seq "   abc d e f  ")))
     i x y))
  "abc d e f  " 2 1 2)

;;; Error cases

(deftest string-left-trim.error.1
  (classify-error (string-left-trim))
  program-error)

(deftest string-left-trim.error.2
  (classify-error (string-left-trim "abc"))
  program-error)

(deftest string-left-trim.error.3
  (classify-error (string-left-trim "abc" "abcdddabc" nil))
  program-error)
