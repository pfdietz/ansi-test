;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun May 11 13:46:44 2003
;;;; Contains: Tests of NO-APPLICABLE-METHOD

(in-package :cl-test)

(defgeneric no-app-meth-gf-01 (x))

(deftest no-applicable-method.2
  (handler-case
   (progn (no-app-meth-gf-01 'x) :bad)
   (error () :good))
  :good)

(defparameter *no-app-meth-gf-02*
  (defgeneric no-app-meth-gf-02 (x)))

(defmethod no-applicable-method ((x (eql *no-app-meth-gf-02*)) &rest args)
  (declare (ignore args x))
  :good)

(deftest no-applicable-method.3
  (no-app-meth-gf-02 (cons 'a 'b))
  :good)

(defparameter *no-app-meth-gf-03*
  (defgeneric no-app-meth-gf-03 (x)))

(defmethod no-applicable-method ((x (eql *no-app-meth-gf-03*)) &rest args)
  (declare (ignore args x))
  :no-method-found)

(defmethod no-app-meth-gf-03 ((x integer)) :good)

(deftest no-applicable-method.4
  (no-app-meth-gf-03 (cons 'a 'b))
  :no-method-found)

(deftest no-applicable-method.5
  (no-app-meth-gf-03 100)
  :good)

(deftest no-applicable-method.6
  (no-app-meth-gf-03 100000000000000000)
  :good)

(defparameter *no-app-meth-gf-04*
  (defgeneric no-app-meth-gf-04 (&rest args)))

(defmethod no-applicable-method ((x (eql *no-app-meth-gf-04*)) &rest args)
  (and (eql x *no-app-meth-gf-04*)
       (copy-list args)))

(deftest no-applicable-method.7
  (no-app-meth-gf-04 'a 'b 'c 'd)
  (a b c d))

(defparameter *no-app-meth-gf-05*
  (defgeneric no-app-meth-gf-05 (x &key)
    (:method ((x symbol) &key) x)))

(defmethod no-applicable-method ((x (eql *no-app-meth-gf-05*)) &rest args)
  (and (eql x *no-app-meth-gf-05*)
       (copy-list args)))

(deftest no-applicable-method.8
  (no-app-meth-gf-05 'a)
  a)

;;; From section 7.6.6: 'Calling no-applicable-method takes precedence
;;;   over checking for acceptable keyword arguments'

(deftest no-applicable-method.9
  (no-app-meth-gf-05 10 :nonsense t)
  (10 :nonsense t))
