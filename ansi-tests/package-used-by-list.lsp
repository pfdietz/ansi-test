;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Feb 22 06:56:28 2004
;;;; Contains: Tests of PACKAGE-USED-BY-LIST

(in-package :cl-test)

;;; Most tests of this function are in files for other package-related operators

(deftest package-used-by-list.error.1
  (signals-error (package-used-by-list) program-error)
  t)

(deftest package-used-by-list.error.2
  (signals-error (package-used-by-list "CL" nil) program-error)
  t)

