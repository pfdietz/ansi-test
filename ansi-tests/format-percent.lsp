;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jul 27 23:47:44 2004
;;;; Contains: Tests of format with ~% directive

(in-package :cl-test)

(deftest format.%.1
  (format nil "~%")
  #.(string #\Newline))

(deftest format.%.2
  (loop for i from 0 to 100
	for s1 = (make-string i :initial-element #\Newline)
	for s2 = (format nil (format nil "~~~D%" i))
	unless (string= s1 s2)
	collect i)
  nil)

(deftest format.%.3
  (format nil "~v%" nil)
  "
")

(deftest format.%.4
  (format nil "~V%" 1)
  #.(string #\Newline))

(deftest format.%.5
  (loop for i from 0 to 100
	for s1 = (make-string i :initial-element #\Newline)
	for s2 = (format nil "~v%" i)
	unless (string= s1 s2)
	collect i)
  nil)

(deftest format.%.6
  (loop for i from 0 to (min (- call-arguments-limit 3) 100)
	for args = (make-list i)
	for s1 = (make-string i :initial-element #\Newline)
	for s2 = (apply #'format nil "~#%" args)
	unless (string= s1 s2)
	collect i)
  nil)

