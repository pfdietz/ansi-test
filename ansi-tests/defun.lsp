;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Feb 16 23:40:32 2003
;;;; Contains: Tests of DEFUN

(in-package :cl-test)

;;; DEFUN is used extensively elsewhere, so I'm just putting error
;;; case tests here

#|
(deftest defun.error.1
  (classify-error (defun))
  program-error)

(deftest defun.error.2
  (classify-error (defun ignored-defun-name))
  program-error)
|#
