;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Nov 29 05:06:57 2003
;;;; Contains: Tests of the function PATHNAME

(in-package :cl-test)

(deftest pathname.1
  (loop for x in *pathnames*
	always (eq x (pathname x)))
  t)

;;; More here

;;; Error tests

(deftest pathname.error.1
  (classify-error (pathname))
  program-error)

(deftest pathname.error.2
  (classify-error (pathname (first *pathnames*) nil))
  program-error)