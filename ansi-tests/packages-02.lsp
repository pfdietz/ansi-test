;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 25 07:50:39 1998
;;;; Contains: Package test code, aprt 02

(in-package :cl-test)
(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; find-package

(deftest find-package-1
    (let ((p (find-package "CL"))
	  (p2 (find-package "COMMON-LISP")))
      (and p p2 (eq p p2)))
  t)

(deftest find-package-2
    (let ((p (find-package "CL-USER"))
	  (p2 (find-package "COMMON-LISP-USER")))
      (and p p2 (eq p p2)))
  t)

(deftest find-package-3
    (let ((p (find-package "KEYWORD")))
      (and p (eq p (symbol-package :test))))
  t)

(deftest find-package-4
    (let ((p (ignore-errors (find-package "A"))))
      (if (packagep p)
	  t
	p))
  t)

(deftest find-package-5
    (let ((p (ignore-errors (find-package #\A))))
      (if (packagep p)
	  t
	p))
  t)

(deftest find-package-6
    (let ((p (ignore-errors (find-package "B"))))
      (if (packagep p)
	  t
	p))
  t)

(deftest find-package-7
    (let ((p (ignore-errors (find-package #\B))))
      (if (packagep p)
	  t
	p))
  t)

(deftest find-package-8
    (let ((p (ignore-errors (find-package "Q")))
	  (p2 (ignore-errors (find-package "A"))))
      (and (packagep p)
	   (packagep p2)
	   (eq p p2)))
  t)

(deftest find-package-9
    (let ((p (ignore-errors (find-package "A")))
	  (p2 (ignore-errors (find-package "B"))))
      (eq p p2))
  nil)

(deftest find-package-10
    (let ((p (ignore-errors (find-package #\Q)))
	  (p2 (ignore-errors (find-package "Q"))))
      (and (packagep p)
	   (eq p p2)))
  t)

(deftest find-package-11
    (handler-case
	(let ((cl (find-package "CL")))
	  (let ((cl2 (find-package cl)))
	    (and (packagep cl)
		 (eq cl cl2))))
      (error (c) c))
  t)


