;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Mar 27 21:29:53 2003
;;;; Contains: Tests for ENSURE-GENERIC-FUNCTION

(in-package :cl-test)

(deftest ensure-generic-function.1
  (subtypep* (classify-error (ensure-generic-function 'car)) 'error)
  t t)

(deftest ensure-generic-function.2
  (subtypep* (classify-error (ensure-generic-function 'defclass)) 'error)
  t t)

(deftest ensure-generic-function.3
  (subtypep* (classify-error (ensure-generic-function 'tagbody)) 'error)
  t t)

(deftest ensure-generic-function.4
  (let ((f 'egf-fun-4))
    (when (fboundp f) (fmakunbound f))
    (values
     (fboundp f)
     (notnot-mv (typep (ensure-generic-function f) 'generic-function))
     (notnot-mv (typep (ensure-generic-function f) 'generic-function))
     (notnot-mv (typep (symbol-function f) 'generic-function))))
  nil t t t)

(deftest ensure-generic-function.5
  (let ((f 'egf-fun-5))
    (when (fboundp f) (fmakunbound f))
    (values
     (fboundp f)
     (notnot-mv (typep (ensure-generic-function f :lambda-list '(a b c))
		       'generic-function))
     ;; Test of incongruent generic function lambda list when no
     ;; methods exist
     (notnot-mv (typep (ensure-generic-function f :lambda-list '(x y))
		       'generic-function))
     (notnot-mv (typep (symbol-function f) 'generic-function))))
  nil t t t)

(deftest ensure-generic-function.6
  (let ((f 'egf-fun-6))
    (when (fboundp f) (fmakunbound f))
    (values
     (fboundp f)
     (notnot-mv (typep (ensure-generic-function f :lambda-list '(a b c))
		       'generic-function))
     (notnot-mv (eval `(defmethod ,f ((a t)(b t)(c t)) (list a b c))))
     ;; Test of incongruent generic function lambda list when no
     ;; methods exist
     (subtypep*
      (classify-error** `(ensure-generic-function ',f :lambda-list '(x y)))
      'error)))
  nil t t t)



