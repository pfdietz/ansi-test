;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Oct  3 20:08:26 2002
;;;; Contains: Tests for STRING-CAPITALIZE

(in-package :cl-test)

(deftest string-capitalize.1
  (let ((s "abCd"))
    (values (string-capitalize s) s))
  "Abcd"
  "abCd")


(deftest string-capitalize.2
  (let ((s "0adA2Cdd3wXy"))
    (values (string-capitalize s) s))
  "0ada2cdd3wxy"
  "0adA2Cdd3wXy")

(deftest string-capitalize.3
  (let ((s "1a"))
    (values (string-capitalize s) s))
  "1a"
  "1a")

(deftest string-capitalize.4
  (let ((s "a1a"))
    (values (string-capitalize s) s))
  "A1a"
  "a1a")

(deftest string-capitalize.5
  (let ((s #\a))
    (values (string-capitalize s) s))
  "A"
  #\a)

(deftest string-capitalize.6
  (let ((s '|abcDe|))
    (values (string-capitalize s) (symbol-name s)))
  "Abcde"
  "abcDe")

(deftest string-capitalize.7
  (let ((s "ABCDEF"))
    (values
     (loop for i from 0 to 5
	   collect (string-capitalize s :start i))
     s))
  ("Abcdef" "ABcdef" "ABCdef" "ABCDef" "ABCDEf" "ABCDEF")
  "ABCDEF")

(deftest string-capitalize.8
  (let ((s "ABCDEF"))
    (values
     (loop for i from 0 to 5
	   collect (string-capitalize s :start i :end nil))
     s))
  ("Abcdef" "ABcdef" "ABCdef" "ABCDef" "ABCDEf" "ABCDEF")
  "ABCDEF")

(deftest string-capitalize.9
  (let ((s "ABCDEF"))
    (values
     (loop for i from 0 to 6
	   collect (string-capitalize s :end i))
     s))
  ("ABCDEF" "ABCDEF" "AbCDEF" "AbcDEF" "AbcdEF" "AbcdeF" "Abcdef")
  "ABCDEF")

(deftest string-capitalize.10
  (let ((s "ABCDEF"))
    (values
     (loop for i from 0 to 5
	   collect (loop for j from i to 6
			 collect (string-capitalize s :start i :end j)))
     s))
  (("ABCDEF" "ABCDEF" "AbCDEF" "AbcDEF" "AbcdEF" "AbcdeF" "Abcdef")
   ("ABCDEF" "ABCDEF" "ABcDEF" "ABcdEF" "ABcdeF" "ABcdef")
   ("ABCDEF" "ABCDEF" "ABCdEF" "ABCdeF" "ABCdef")
   ("ABCDEF" "ABCDEF" "ABCDeF" "ABCDef")
   ("ABCDEF" "ABCDEF" "ABCDEf")
   ("ABCDEF" "ABCDEF"))
  "ABCDEF")
