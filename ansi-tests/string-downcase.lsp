;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Sep 30 21:41:59 2002
;;;; Contains: Tests for STRING-DOWNCASE

(in-package :cl-test)

(deftest string-downcase.1
  (let ((s "A"))
    (values (string-downcase s) s))
  "a" "A")

(deftest string-downcase.2
  (let ((s "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))
    (values (string-downcase s) s))
  "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz"
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

(deftest string-downcase.3
  (let ((s "0123456789!@#$%^&*()_-+=|\\{}[]:\";'<>?,./ "))
    (values (string-downcase s) s))
  "0123456789!@#$%^&*()_-+=|\\{}[]:\";'<>?,./ "
  "0123456789!@#$%^&*()_-+=|\\{}[]:\";'<>?,./ ")

(deftest string-downcase.4
  (string-downcase #\A)
  "a")

(deftest string-downcase.5
  (let ((sym '|A|))
    (values (string-downcase sym) sym))
  "a" |A|)

(deftest string-downcase.6
  (let ((s (make-array 6 :element-type 'character
		       :initial-contents '(#\A #\B #\C #\D #\E #\F))))
    (values (string-downcase s) s))
  "abcdef"
  "ABCDEF")

(deftest string-downcase.7
  (let ((s (make-array 6 :element-type 'standard-char
		       :initial-contents '(#\A #\B #\7 #\D #\E #\F))))
    (values (string-downcase s) s))
  "ab7def"
  "AB7DEF")

;; Tests with :start, :end

(deftest string-downcase.8
  (let ((s "ABCDEF"))
    (values
     (loop for i from 0 to 6
	   collect (string-downcase s :start i))
     s))
  ("abcdef" "Abcdef" "ABcdef" "ABCdef" "ABCDef" "ABCDEf" "ABCDEF")
  "ABCDEF")

(deftest string-downcase.9
  (let ((s "ABCDEF"))
    (values
     (loop for i from 0 to 6
	   collect (string-downcase s :start i :end nil))
     s))
  ("abcdef" "Abcdef" "ABcdef" "ABCdef" "ABCDef" "ABCDEf" "ABCDEF")
  "ABCDEF")

(deftest string-downcase.10
  (let ((s "ABCDE"))
    (values
     (loop for i from 0 to 4
	   collect (loop for j from i to 5
			 collect (string-invertcase
				  (string-downcase s :start i :end j))))
     s))
  (("abcde" "Abcde" "ABcde" "ABCde" "ABCDe" "ABCDE")
   ("abcde" "aBcde" "aBCde" "aBCDe" "aBCDE")
   ("abcde" "abCde" "abCDe" "abCDE")
   ("abcde" "abcDe" "abcDE")
   ("abcde" "abcdE"))
  "ABCDE")

;;; Error cases

(deftest string-downcase.error.1
  (classify-error (string-downcase))
  program-error)

(deftest string-downcase.error.2
  (classify-error (string-downcase (copy-seq "abc") :bad t))
  program-error)

(deftest string-downcase.error.3
  (classify-error (string-downcase (copy-seq "abc") :start))
  program-error)

(deftest string-downcase.error.4
  (classify-error (string-downcase (copy-seq "abc") :bad t
				      :allow-other-keys nil))
  program-error)

(deftest string-downcase.error.5
  (classify-error (string-downcase (copy-seq "abc") :end))
  program-error)

(deftest string-downcase.error.6
  (classify-error (string-downcase (copy-seq "abc") 1 2))
  program-error)
