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

;;; DEFCONSTANT

(def-error-test defconstant.1 (defconstant))
(def-error-test defconstant.2 (defconstant . foo))
(def-error-test defconstant.3 (defconstant #.(gensym)))
(def-error-test defconstant.4 (defconstant #.(gensym) . foo))
(def-error-test defconstant.5 (defconstant #.(gensym) nil . foo))
(def-error-test defconstant.6 (defconstant #.(gensym) nil "foo" . bar))

(def-all-error-test defconstant.7 'symbolp
  #'(lambda (x) `(defconstant ,x nil)))

(def-all-error-test defconstant.8 'stringp
  #'(lambda (x) `(defconstant ,(gensym) nil ,x)))

;;; DEFPARAMETER

(def-error-test defparameter.1 (defparameter))
(def-error-test defparameter.2 (defparameter . foo))
(def-error-test defparameter.3 (defparameter #.(gensym)))
(def-error-test defparameter.4 (defparameter #.(gensym) . foo))
(def-error-test defparameter.5 (defparameter #.(gensym) nil . foo))
(def-error-test defparameter.6 (defparameter #.(gensym) nil "foo" . bar))

(def-all-error-test defparameter.7 'symbolp
  #'(lambda (x) `(defparameter ,x nil)))

(def-all-error-test defparameter.8 'stringp
  #'(lambda (x) `(defparameter ,(gensym) nil ,x)))

;;; DEFVAR

(def-error-test defvar.1 (defvar))
(def-error-test defvar.2 (defvar . foo))
(def-error-test defvar.4 (defvar #.(gensym) . foo))
(def-error-test defvar.5 (defvar #.(gensym) nil . foo))
(def-error-test defvar.6 (defvar #.(gensym) nil "foo" . bar))

(def-all-error-test defvar.7 'symbolp
  #'(lambda (x) `(defvar ,x nil)))

(def-all-error-test defvar.8 'stringp
  #'(lambda (x) `(defvar ,(gensym) nil ,x)))

;;; DESTRUCTURING-BIND

(def-error-test destructuring-bind.1 (destructuring-bind))
(def-error-test destructuring-bind.2 (destructuring-bind x))
(def-all-error-test destructuring-bind.3
  (typef '(or symbol cons))
  #'(lambda (x) `(destructuring-bind ,x nil)))

;;; LET

(def-error-test let.1 (let))
(def-error-test let.2 (let . x))
(def-all-error-test let.3 'listp #'(lambda (x) `(let ,x nil)))
(def-error-test let.4 (let () . x))
(def-error-test let.5 (let (x . 1) nil))
(def-error-test let.6 (let ((x) . y) nil))
(def-error-test let.7 (let ((x 1 . 2)) nil))
(def-error-test let.8 (let ((x 1 2)) nil))
(def-error-test let.9 (let ((x 1) (x 2)) x))
(def-error-test let.10 (let ((t 1)) t))
(def-all-error-test let.11 (typef '(or cons symbol))
  #'(lambda (x) `(let (,x) nil)))
(def-all-error-test let.12 'symbolp
  #'(lambda (x) `(let ((,x)) nil)))

;;; LET*

(def-error-test let*.1 (let*))
(def-error-test let*.2 (let* . x))
(def-all-error-test let*.3 'listp #'(lambda (x) `(let* ,x nil)))
(def-error-test let*.4 (let* () . x))
(def-error-test let*.5 (let* (x . 1) nil))
(def-error-test let*.6 (let* ((x) . y) nil))
(def-error-test let*.7 (let* ((x 1 . 2)) nil))
(def-error-test let*.8 (let* ((x 1 2)) nil))
(def-error-test let*.10 (let* ((t 1)) t))
(def-all-error-test let*.11 (typef '(or cons symbol))
  #'(lambda (x) `(let* (,x) nil)))
(def-all-error-test let*.12 'symbolp
  #'(lambda (x) `(let* ((,x)) nil)))

;;; PROGV

(def-error-test progv.1 (progv))
(def-error-test progv.2 (progv '(a)))
(def-all-error-test progv.3 'listp '(progv x nil nil))
(def-all-error-test progv.4 'listp '(progv '(a) x nil))

