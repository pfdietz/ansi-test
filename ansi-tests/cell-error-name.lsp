;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jan 27 22:36:48 2003
;;;; Contains: Tests of CELL-ERROR-NAME

(in-package :cl-test)

(deftest cell-error-name.1
  (handler-case
   (eval 'my-unbound-variable)
   (cell-error (c) (cell-error-name c)))
  my-unbound-variable)

(deftest cell-error-name.2
  (handler-case
   (eval '(my-undefined-function))
   (cell-error (c) (cell-error-name c)))
  my-undefined-function)

(deftest cell-error-name.3
  (cell-error-name (make-condition 'unbound-variable :name 'x))
  x)
  
(deftest cell-error-name.4
  (cell-error-name (make-condition 'undefined-function :name 'f))
  f)
  
(deftest cell-error-name.5
  (cell-error-name (make-condition 'unbound-slot :name 's))
  s)
  
;;; Need test raising condition unbound-slot


(deftest cell-error-name.error.1
  (classify-error (cell-error-name))
  program-error)

(deftest cell-error-name.error.2
  (classify-error (cell-error-name (make-condition 'unbound-variable :name 'foo) nil))
  program-error)
