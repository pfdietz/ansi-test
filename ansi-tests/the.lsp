;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue May  6 06:48:48 2003
;;;; Contains: Tests of THE

(in-package :cl-test)

(deftest the.1
  (the (values) (values)))

(deftest the.2
  (the (values) 'a)
  a)

(deftest the.3
  (loop for e in *universe*
	for x = (multiple-value-list (eval `(the (values) (quote ,e))))
	unless (and x (not (cdr x)) (eql (car x) e))
	collect e)
  nil)

(deftest the.4
  (loop for e in *universe*
	for x = (multiple-value-list (eval `(the ,(type-of e) (quote ,e))))
	unless (and x (not (cdr x)) (eql (car x) e))
	collect e)
  nil)

(deftest the.5
  (loop for e in *universe*
	for x = (multiple-value-list (eval `(the (values ,(type-of e))
					      (quote ,e))))
	unless (and x (not (cdr x)) (eql (car x) e))
	collect e)
  nil)

(deftest the.6
  (loop for e in *universe*
	for x = (multiple-value-list (eval `(the (values ,(type-of e) t)
					      (quote ,e))))
	unless (and x (not (cdr x)) (eql (car x) e))
	collect e)
  nil)

(deftest the.7
  (loop for e in *universe*
	for x = (multiple-value-list (eval `(the (values ,(type-of e))
					      (values (quote ,e) :ignored))))
	unless (and (eql (length x) 2)
		    (eql (car x) e)
		    (eql (cadr x) :ignored))
	collect e)
  nil)

(deftest the.8
  (loop for e in *universe*
	when (and (constantp e)
		  (not (eql (eval `(the ,(type-of e) ,e)) e)))
	collect e)
  nil)

(deftest the.9
  (loop for e in *universe*
	when (and (constantp e)
		  (not (eql (eval `(the ,(class-of e) ,e)) e)))
	collect e)
  nil)

(deftest the.10
  (loop for e in *universe*
	unless (eql (eval `(the ,(class-of e) ',e)) e)
	collect e)
  nil)

