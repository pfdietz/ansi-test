;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 17:18:01 2003
;;;; Contains: Tests of DEFSETF

(in-package :cl-test)

;;; Need to add non-error tests

(def-macro-test defsetf.error.1 (defsetf nonexistent-access-fn
				  nonexistent-update-fn))
