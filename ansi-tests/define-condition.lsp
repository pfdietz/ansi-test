;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar  8 22:38:53 2003
;;;; Contains: Tests of DEFINE-CONDITION (part 1)

(in-package :cl-test)

;;;

(define-condition-with-tests cond-1 nil nil)

(define-condition-with-tests cond-2 (condition) nil)

