;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Oct  3 21:38:49 2002
;;;; Contains: Tests for NSTRING-CAPITALIZE

(in-package :cl-test)

(deftest nstring-capitalize.1
  (let* ((s (copy-seq "abCd"))
	 (s2 (nstring-capitalize s)))
    (values (eqt s s2) s))
  t "Abcd")

(deftest nstring-capitalize.2
  (let* ((s (copy-seq "0adA2Cdd3wXy"))
	 (s2 (nstring-capitalize s)))
    (values (eqt s s2) s))
  t "0ada2cdd3wxy")

(deftest nstring-capitalize.3
  (let* ((s (copy-seq "1a"))
	 (s2 (nstring-capitalize s)))
    (values (eqt s s2) s))
  t "1a")

(deftest nstring-capitalize.4
  (let* ((s (copy-seq "a1a"))
	 (s2 (nstring-capitalize s)))
    (values (eqt s s2) s))
  t "A1a")

(deftest nstring-capitalize.7
  (let ((s "ABCDEF"))
     (loop for i from 0 to 5
	   collect (nstring-capitalize (copy-seq s) :start i)))
  ("Abcdef" "ABcdef" "ABCdef" "ABCDef" "ABCDEf" "ABCDEF"))

(deftest nstring-capitalize.8
  (let ((s "ABCDEF"))
     (loop for i from 0 to 5
	   collect (nstring-capitalize (copy-seq s) :start i :end nil)))
  ("Abcdef" "ABcdef" "ABCdef" "ABCDef" "ABCDEf" "ABCDEF"))

(deftest nstring-capitalize.9
  (let ((s "ABCDEF"))
     (loop for i from 0 to 6
	   collect (nstring-capitalize (copy-seq s) :end i)))
  ("ABCDEF" "ABCDEF" "AbCDEF" "AbcDEF" "AbcdEF" "AbcdeF" "Abcdef"))

(deftest nstring-capitalize.10
  (let ((s "ABCDEF"))
    (loop for i from 0 to 5
	  collect (loop for j from i to 6
			collect (nstring-capitalize (copy-seq s)
						    :start i :end j))))
  (("ABCDEF" "ABCDEF" "AbCDEF" "AbcDEF" "AbcdEF" "AbcdeF" "Abcdef")
   ("ABCDEF" "ABCDEF" "ABcDEF" "ABcdEF" "ABcdeF" "ABcdef")
   ("ABCDEF" "ABCDEF" "ABCdEF" "ABCdeF" "ABCdef")
   ("ABCDEF" "ABCDEF" "ABCDeF" "ABCDef")
   ("ABCDEF" "ABCDEF" "ABCDEf")
   ("ABCDEF" "ABCDEF")))
