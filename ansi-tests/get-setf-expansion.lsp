;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jan 13 17:05:17 2003
;;;; Contains: Tests for GET-SETF-EXPANSION

(in-package :cl-test)

(deftest get-setf-expansion.error.1
  (signals-error (get-setf-expansion) program-error)
  t)

(deftest get-setf-expansion.error.2
  (signals-error (get-setf-expansion 'x nil nil) program-error)
  t)

;;; FIXME
;;; Tests for proper behavior will go here
