;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jan 13 15:27:51 2003
;;;; Contains: Tests for FDEFINITION

(in-package :cl-test)

;;; Error cases

(deftest fdefinition.error.1
  (signals-error (fdefinition) program-error)
  t)

(deftest fdefinition.error.2
  (signals-error (fdefinition 'cons nil) program-error)
  t)

(deftest fdefinition.error.3
  (let ((v (gensym)))
    (eval `(signals-error (fdefinition ',v) undefined-function
			  :name ,v)))
  t)

(deftest fdefinition.error.4
  (signals-error (fdefinition 10) type-error :name 10)
  t)

(deftest fdefinition.error.5
  (let ((fn `(setf ,(gensym))))
    (eval `(signals-error (fdefinition ',fn) undefined-function
			  :name ,fn)))
  t)

(deftest fdefinition.error.6
  (signals-error (locally (fdefinition 10) t) type-error)
  t)

;;; Non-error cases

(deftest fdefinition.1
  (let ((fun (fdefinition 'cons)))
    (funcall fun 'a 'b))
  (a . b))

(deftest fdefinition.2
  (progn
    (fdefinition 'cond)
    :good)
  :good)

(deftest fdefinition.3
  (progn
    (fdefinition 'setq)
    :good)
  :good)

(deftest fdefinition.4
  (let ((sym (gensym)))
    (values
     (fboundp sym)
     (progn
       (setf (fdefinition sym) (fdefinition 'cons))
       (funcall (symbol-function sym) 'a 'b))
     (notnot (fboundp sym))))
  nil
  (a . b)
  t)

(deftest fdefinition.5
  (let* ((sym (gensym))
	 (fname (list 'setf sym)))
    (values
     (fboundp fname)
     (progn
       (setf (fdefinition fname) (fdefinition 'cons))
       (eval `(setf (,sym 'a) 'b)))
     (notnot (fboundp fname))))
  nil
  (b . a)
  t)

(deftest fdefinition.order.1
  (let ((i 0))
    (fdefinition (progn (incf i) 'setq))
    i)
  1)

