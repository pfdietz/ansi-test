;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Feb 22 06:55:17 2004
;;;; Contains: Tests of PACKAGE-SHADOWING-SYMBOLS

(in-package :cl-test)

;;; Most tests of this function are in files for other package-related operators

(deftest package-shadowing-symbols.error.1
  (signals-error (package-shadowing-symbols) program-error)
  t)

(deftest package-shadowing-symbols.error.2
  (signals-error (package-shadowing-symbols "CL" nil) program-error)
  t)

