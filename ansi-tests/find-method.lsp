;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jun  3 21:12:03 2003
;;;; Contains: Tests for FIND-METHOD

(in-package :cl-test)

(defgeneric find-method-gf-01 (x))
(defparameter *find-method-gf-01-method1*
  (defmethod find-method-gf-01 ((x integer)) 'a))
(defparameter *find-method-gf-01-method2*
  (defmethod find-method-gf-01 ((x rational)) 'b))
(defparameter *find-method-gf-01-method3*
  (defmethod find-method-gf-01 ((x real)) 'c))
(defparameter *find-method-gf-01-method4*
  (defmethod find-method-gf-01 ((x t)) 'd))

(deftest find-method.2
  (eqt (find-method #'find-method-gf-01 nil (list (find-class 'integer)))
       *find-method-gf-01-method1*)
  t)

(deftest find-method.4
  (eqt (find-method #'find-method-gf-01 nil (list (find-class 'rational)))
       *find-method-gf-01-method2*)
  t)

(deftest find-method.6
  (eqt (find-method #'find-method-gf-01 nil (list (find-class 'real)))
       *find-method-gf-01-method3*)
  t)

(deftest find-method.8
  (eqt (find-method #'find-method-gf-01 nil (list (find-class t)))
       *find-method-gf-01-method4*)
  t)

(deftest find-method.9
  (find-method #'find-method-gf-01 (list :around) (list (find-class t))
	       nil)
  nil)

(deftest find-method.10
  (find-method #'find-method-gf-01 (list :after)
	       (list (find-class 'integer)) nil)
  nil)

(deftest find-method.11
  (find-method #'find-method-gf-01 (list :before) (list (find-class 'real))
	       nil)
  nil)

(deftest find-method.error.1
  (classify-error (find-method))
  program-error)

(deftest find-method.error.2
  (classify-error (find-method #'find-method-gf-01))
  program-error)

(deftest find-method.error.3
  (classify-error (find-method #'find-method-gf-01 nil))
  program-error)

(deftest find-method.error.4
  (classify-error
   (find-method #'find-method-gf-01 nil (list (find-class 'integer)) nil nil))
  program-error)

(deftest find-method.error.5
  (handler-case
   (find-method #'find-method-gf-01 nil (list (find-class 'symbol)))
   (error () :error))
  :error)

(deftest find-method.error.6
  (handler-case
   (find-method #'find-method-gf-01 nil (list (find-class 'symbol)) 'x)
   (error () :error))
  :error)

(deftest find-method.error.7
  (handler-case
   (find-method #'find-method-gf-01 nil nil)
   (error () :error))
  :error)

(deftest find-method.error.8
  (handler-case
   (find-method #'find-method-gf-01 nil (list (find-class 'integer)
					      (find-class t)))
   (error () :error))
  :error)

(deftest find-method.error.9
  (handler-case
   (find-method #'find-method-gf-01 nil nil nil)
   (error () :error))
  :error)

(deftest find-method.error.10
  (handler-case
   (find-method #'find-method-gf-01 nil (list (find-class 'integer)
					      (find-class t))
		nil)
   (error () :error))
  :error)









