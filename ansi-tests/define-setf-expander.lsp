;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 17:19:35 2003
;;;; Contains: Tests of DEFINE-SETF-EXPANDER

(in-package :cl-test)

;;; Need to add non-error tests

(def-macro-test define-setf-expander.error.1
  (define-setf-expander nonexistent-access-fn (x)))
