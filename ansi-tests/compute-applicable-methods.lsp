;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jun  2 06:40:41 2003
;;;; Contains: Tests for COMPUTE-APPLICABLE-METHODS

(in-package :cl-test)

(defgeneric cam-gf-01 (x y))

(defparameter *cam-gf-01-method1*
  (defmethod cam-gf-01 ((x integer) (y integer)) 1))

(defparameter *cam-gf-01-method2*
  (defmethod cam-gf-01 ((x integer) (y t)) 2))

(defparameter *cam-gf-01-method3*
  (defmethod cam-gf-01 ((x t) (y integer)) 3))

(defparameter *cam-gf-01-method4*
  (defmethod cam-gf-01 ((x t) (y t)) 4))

(deftest compute-applicable-methods.1
  (let ((methods (compute-applicable-methods #'cam-gf-01 (list 1 2))))
    (equalt methods
	    (list *cam-gf-01-method1* *cam-gf-01-method2*
		  *cam-gf-01-method3* *cam-gf-01-method4*)))
  t)

(deftest compute-applicable-methods.2
  (let ((methods (compute-applicable-methods #'cam-gf-01 (list 1 'x))))
    (equalt methods
	    (list *cam-gf-01-method2* *cam-gf-01-method4*)))
  t)

(deftest compute-applicable-methods.3
  (let ((methods (compute-applicable-methods #'cam-gf-01 (list 'x 10))))
    (equalt methods
	    (list *cam-gf-01-method3* *cam-gf-01-method4*)))
  t)

(deftest compute-applicable-methods.4
  (let ((methods (compute-applicable-methods #'cam-gf-01 (list 'x 'y))))
    (equalt methods (list *cam-gf-01-method4*)))
  t)


(defgeneric cam-gf-02 (x))

(deftest compute-applicable-methods.5
  (compute-applicable-methods #'cam-gf-02 '(1))
  nil)


;;; Error tests

(deftest compute-applicable-methods.error.1
  (classify-error (compute-applicable-methods))
  program-error)

(deftest compute-applicable-methods.error.2
  (classify-error (compute-applicable-methods #'cam-gf-01))
  program-error)

(deftest compute-applicable-methods.error.3
  (classify-error (compute-applicable-methods #'cam-gf-01 '(1 2) nil))
  program-error)



