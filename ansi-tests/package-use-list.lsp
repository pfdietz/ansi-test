;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Feb 22 06:55:56 2004
;;;; Contains: Tests of PACKAGE-USE-LIST

(in-package :cl-test)

;;; Most tests of this function are in files for other package-related operators

(deftest package-use-list.error.1
  (signals-error (package-use-list) program-error)
  t)

(deftest package-use-list.error.2
  (signals-error (package-use-list "CL" nil) program-error)
  t)
