;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 25 08:07:31 1998
;;;; Contains: Package test code (common code)

(in-package :cl-test)
(declaim (optimize (safety 3)))
 
(defpackage "A"
  (:use)
  (:nicknames "Q")
  (:export "FOO"))

(defpackage "B"
  (:use "A")
  (:export "BAR"))

(defpackage "DS1"
  (:use)
  (:intern "C" "D")
  (:export "A" "B"))

(defpackage "DS2"
  (:use)
  (:intern "E" "F")
  (:export "G" "H" "A"))

(defpackage "DS3"
  (:shadow "B")
  (:shadowing-import-from "DS1" "A")
  (:use "DS1" "DS2")
  (:export "A" "B" "G" "I" "J" "K")
  (:intern "L" "M"))

(defpackage "DS4"
  (:shadowing-import-from "DS1" "B")
  (:use "DS1" "DS3")
  (:intern "X" "Y" "Z")
  (:import-from "DS2" "F"))9

(defun sort-package-list (x)
  (sort (copy-list x)
	#'string<
	:key #'package-name))

(defun sort-symbols (sl)
  (sort (copy-list sl)
	#'(lambda (x y)
	    (or
	     (string< (symbol-name x)
		      (symbol-name y))
	     (and (string= (symbol-name x)
			   (symbol-name y))
		  (string< (package-name (symbol-package x))
			   (package-name (symbol-package y))))))))

(defun num-symbols-in-package (p)
  (let ((num 0))
    (declare (fixnum num))
    (do-symbols (s p num)
      (declare (ignore s))
      (incf num))))

(defun num-external-symbols-in-package (p)
  (let ((num 0))
    (declare (fixnum num))
    (do-external-symbols (s p num)
      (declare (ignore s))
      (incf num))))
