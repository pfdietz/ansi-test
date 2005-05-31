;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue May 31 08:08:49 2005
;;;; Contains: Tests of non-ANSI exceptional situations from CLHS section 5, part 2

(in-package :ba-test)

(compile-and-load "ba-aux.lsp")

;;; FUNCALL

(def-all-error-test funcall.1 'function-designator-p '(funcall x))
(def-error-test funcall.2 (funcall cons 1 . 2))

;;; FUNCTION

(def-error-test function.1 (function))
(def-error-test function.2 (function . cons))
(def-error-test function.3 (function cons . foo))
(def-error-test function.4 (function cons nil))
(def-all-error-test function.5 'function-name-p '(function x))
(def-all-error-test function.6
  (constantly nil) #'(lambda (x) `(function ,x))
  :vals cl-test::*cl-macro-symbols*)
(def-all-error-test function.7
  (constantly nil) #'(lambda (x) `(function ,x))
  :vals cl-test::*cl-special-operator-symbols*)
(def-error-test function.8 (macrolet ((%m () nil)) #'%m))

;;; FUNCTION-LAMBDA-EXPRESSION

(def-all-error-test function-lambda-expression.1
  'functionp '(function-lambda-expression x))


