;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jun 16 19:40:32 2003
;;;; Contains: Tests of CLASS-OF

(in-package :cl-test)

;;; Most tests of CLASS-OF are in other files

(deftest class-of.error.1
  (classify-error (class-of))
  program-error)

(deftest class-of.error.2
  (classify-error (class-of nil nil))
  program-error)
