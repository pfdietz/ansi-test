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
	 (s2 (string-right-trim (make-array 4 :initial-contents '(#\a #\b #\c #\d)
				      :element-type 'character
				      :fill-pointer 2)
			  s)))
    (values s s2))
  "abcdaba"
  "abcd")

(deftest string-right-trim.9
  (let* ((s (make-array 7 :initial-contents "abcdaba"
			:element-type 'character
			))
	 (s2 (string-right-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "abcd")

(deftest string-right-trim.10
  (let* ((s (make-array 9 :initial-contents "abcdabadd"
			:element-type 'character
			:fill-pointer 7))
	 (s2 (string-right-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "abcd")

(deftest string-right-trim.11
  (let* ((s (make-array 7 :initial-contents "abcdaba"
			:element-type 'standard-char
			))
	 (s2 (string-right-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "abcd")

(deftest string-right-trim.12
  (let* ((s (make-array 7 :initial-contents "abcdaba"
			:element-type 'base-char
			))
	 (s2 (string-right-trim "ab" s)))
    (values s s2))
  "abcdaba"
  "abcd")

;;; Test that trimming is case sensitive
(deftest string-right-trim.13
  (let* ((s (copy-seq "Aa"))
	 (s2 (string-right-trim "a" s)))
    (values s s2))
  "Aa" "A")

(deftest string-right-trim.14
  (let* ((s '|abcdaba|)
	 (s2 (string-right-trim "ab" s)))
    (values (symbol-name s) s2))
  "abcdaba"
  "abcd")

(deftest string-right-trim.15
  (string-right-trim "abc" "")
  "")

(deftest string-right-trim.16
  (string-right-trim "a" #\a)
  "")

(deftest string-right-trim.17
  (string-right-trim "b" #\a)
  "a")

(deftest string-right-trim.18
  (string-right-trim "" (copy-seq "abcde"))
  "abcde")

(deftest string-right-trim.19
  (string-right-trim "abc" (copy-seq "abcabcabc"))
  "")

(deftest string-right-trim.order.1
  (let ((i 0) x y)
    (values
     (string-right-trim (progn (setf x (incf i)) " ")
		       (progn (setf y (incf i))
			      (copy-seq "   abc d e f  ")))
     i x y))
  "   abc d e f" 2 1 2)

;;; Error cases

(deftest string-right-trim.error.1
  (classify-error (string-right-trim))
  program-error)

(deftest string-right-trim.error.2
  (classify-error (string-right-trim "abc"))
  program-error)

(deftest string-right-trim.error.3
  (classify-error (string-right-trim "abc" "abcdddabc" nil))
  program-error)
