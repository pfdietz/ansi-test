;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Jun  9 07:02:53 2005
;;;; Contains: Separate tests for DEFMETHOD

(in-package :cl-test)

(deftest defmethod.1
  (let ((sym (gensym)))
    (values
     (typep* (eval `(defmethod ,sym (x) (list x))) 'standard-method)
     (typep* (fdefinition sym) 'standard-generic-function)
     (funcall sym 1)))
  t t (1))

(deftest defmethod.2
  (let* ((sym (gensym))
	 (method
	  (eval `(defmethod ,sym ((x integer)) (list x)))))
    (values
     (typep* method 'standard-method)
     (typep* (fdefinition sym) 'standard-generic-function)
     (funcall sym 1)))
  t t (1))

(deftest defmethod.3
  (let* ((sym (gensym))
	 (method
	  (eval `(let ((x 0)) (defmethod ,sym ((x (eql (incf x)))) (list x))))))
    (values
     (typep* method 'standard-method)
     (typep* (fdefinition sym) 'standard-generic-function)
     (funcall sym 1)
     (funcall sym 1)))
  t t (1) (1))


;;; Error cases

;;; Lambda liss not congruent

(deftest defmethod.error.1
  (let ((sym (gensym)))
    (eval `(defgeneric ,sym (x y)))
    (eval `(signals-error (defmethod ,sym ((x t)) x) error)))
  t)

(deftest defmethod.error.2
  (let ((sym (gensym)))
    (eval `(defgeneric ,sym (x y)))
    (eval `(signals-error (defmethod ,sym ((x t) (y t) (z t)) (list x y z)) error)))
  t)

(deftest defmethod.error.3
  (let ((sym (gensym)))
    (eval `(defgeneric ,sym (x y &optional z)))
    (eval `(signals-error (defmethod ,sym ((x t) (y t) (z t)) (list x y z)) error)))
  t)

(deftest defmethod.error.4
  (let ((sym (gensym)))
    (eval `(defgeneric ,sym (x y &optional z)))
    (eval `(signals-error (defmethod ,sym ((x t) (y t) &optional) (list x y)) error)))
  t)

(deftest defmethod.error.5
  (let ((sym (gensym)))
    (eval `(defgeneric ,sym (x y &optional z)))
    (eval `(signals-error (defmethod ,sym ((x t) (y t) &optional z w) (list x y z w)) error)))
  t)

(deftest defmethod.error.6
  (let ((sym (gensym)))
    (eval `(defgeneric ,sym (x &rest z)))
    (eval `(signals-error (defmethod ,sym ((x t)) (list x)) error)))
  t)

(deftest defmethod.error.7
  (let ((sym (gensym)))
    (eval `(defgeneric ,sym (x)))
    (eval `(signals-error (defmethod ,sym ((x t) &rest z) (list x z)) error)))
  t)

(deftest defmethod.error.8
  (let ((sym (gensym)))
    (eval `(defgeneric ,sym (x &key z)))
    (eval `(signals-error (defmethod ,sym ((x t)) (list x)) error)))
  t)

(deftest defmethod.error.9
  (let ((sym (gensym)))
    (eval `(defgeneric ,sym (x)))
    (eval `(signals-error (defmethod ,sym ((x t) &key z) (list x z)) error)))
  t)

(deftest defmethod.error.10
  (let ((sym (gensym)))
    (eval `(defgeneric ,sym (x &key z)))
    (eval `(signals-error (defmethod ,sym ((x t) &key) x) error)))
  t)

(deftest defmethod.error.11
  (let ((sym (gensym)))
    (eval `(defgeneric ,sym (x &key)))
    (eval `(signals-error (defmethod ,sym ((x t)) x) error)))
  t)

(deftest defmethod.error.12
  (let ((sym (gensym)))
    (eval `(defgeneric ,sym (x)))
    (eval `(signals-error (defmethod ,sym ((x t) &key) x) error)))
  t)









