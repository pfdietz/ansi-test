;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Oct  3 21:12:40 2002
;;;; Contains: Tests for NSTRING-UPCASE

(in-package :cl-test)

(deftest nstring-upcase.1
  (let* ((s (copy-seq "a"))
	 (s2 (nstring-upcase s)))
    (values (eqt s s2) s))
  t "A")

(deftest nstring-upcase.2
  (let* ((s (copy-seq "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))
	 (s2 (nstring-upcase s)))
    (values (eqt s s2) s))
  t
  "ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ")  

(deftest nstring-upcase.3
  (let* ((s (copy-seq "0123456789!@#$%^&*()_-+=|\\{}[]:\";'<>?,./ "))
	 (s2 (nstring-upcase s)))
    (values (eqt s s2) s))
  t
  "0123456789!@#$%^&*()_-+=|\\{}[]:\";'<>?,./ ")

(deftest nstring-upcase.6
  (let* ((s (make-array 6 :element-type 'character
			:initial-contents '(#\a #\b #\c #\d #\e #\f)))
	 (s2 (nstring-upcase s)))
    (values (eqt s s2) s))
  t "ABCDEF")

(deftest nstring-upcase.7
  (let* ((s (make-array 6 :element-type 'standard-char
			:initial-contents '(#\a #\b #\7 #\d #\e #\f)))
	 (s2 (nstring-upcase s)))
    (values (eqt s s2) s))
  t "AB7DEF")

;; Tests with :start, :end

(deftest nstring-upcase.8
  (let ((s "abcdef"))
    (loop for i from 0 to 6
	  collect (nstring-upcase (copy-seq s) :start i)))
  ("ABCDEF" "aBCDEF" "abCDEF" "abcDEF" "abcdEF" "abcdeF" "abcdef"))

(deftest nstring-upcase.9
  (let ((s "abcdef"))
    (loop for i from 0 to 6
	  collect 
	  (nstring-upcase (copy-seq s) :start i :end nil)))
  ("ABCDEF" "aBCDEF" "abCDEF" "abcDEF" "abcdEF" "abcdeF" "abcdef"))

(deftest nstring-upcase.10
  (let ((s "abcde"))
     (loop for i from 0 to 4
	   collect (loop for j from i to 5
			 collect (nstring-upcase (copy-seq s)
						 :start i :end j))))
  (("abcde" "Abcde" "ABcde" "ABCde" "ABCDe" "ABCDE")
   ("abcde" "aBcde" "aBCde" "aBCDe" "aBCDE")
   ("abcde" "abCde" "abCDe" "abCDE")
   ("abcde" "abcDe" "abcDE")
   ("abcde" "abcdE")))

  
