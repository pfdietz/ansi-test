;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Jul 28 00:27:00 2004
;;;; Contains: Tests of format directive ~~

(in-package :cl-test)

(deftest format.~.1
  (format nil "~~")
  "~")

(deftest format.~.2
  (loop for i from 0 to 100
	for s = (make-string i :initial-element #\~)
	for s2 = (format nil (format nil "~~~D~~" i))
	unless (string= s s2)
	collect (list i s s2))
  nil)

(deftest format.~.3
  (format nil "~v~" 0)
  "")

(deftest format.~.4
  (loop for i from 0 to 100
	for s = (make-string i :initial-element #\~)
	for s2 = (format nil "~V~" i)
	unless (string= s s2)
	collect (list i s s2))
  nil)

(deftest format.~.5
  (loop for i from 0 to (min (- call-arguments-limit 3) 100)
	for s = (make-string i :initial-element #\~)
	for args = (make-list i)
	for s2 = (apply #'format nil "~#~" args)
	unless (string= s s2)
	collect (list i s s2))
  nil)

