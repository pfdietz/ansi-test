;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Dec 15 21:57:44 2004
;;;; Contains: Tests of SUBTYPEP on FUNCTION types

(in-package :cl-test)

(deftest subtypep-function.1
  (check-all-not-subtypep t '(function (t) t))
  nil)

