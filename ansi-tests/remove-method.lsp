;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun May 11 19:53:37 2003
;;;; Contains: Tests of REMOVE-METHOD

(in-package :cl-test)

(defparameter *remove-meth-gf-01*
  (defgeneric remove-meth-gf-01 (x)))

(defparameter *remove-meth-gf-01-method-t*
  (defmethod remove-meth-gf-01 ((x t)) x))

(defparameter *remove-meth-gf-02*
  (defgeneric remove-meth-gf-02 (x)))

(defparameter *remove-meth-gf-02-method-t*
  (defmethod remove-meth-gf-02 ((x t)) x))

;;; remove method must not signal an error if the method
;;; does not belong to the generic function

(deftest remove-method.1
  (and
   (eqt (remove-method *remove-meth-gf-01* *remove-meth-gf-02-method-t*)
	*remove-meth-gf-01*)
   (remove-meth-gf-01 :good))
  :good)





