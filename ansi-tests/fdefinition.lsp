;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jan 13 15:27:51 2003
;;;; Contains: Tests for FDEFINITION

(in-package :cl-test)

;;; Error cases

(deftest fdefinition.error.1
  (classify-error (fdefinition))
  program-error)

(deftest fdefinition.error.2
  (classify-error (fdefinition 'cons nil))
  program-error)

(deftest fdefinition.error.3
  (classify-error (fdefinition (gensym)))
  undefined-function)

(deftest fdefinition.error.4
  (classify-error (fdefinition 10))
  type-error)

(deftest fdefinition.error.5
  (classify-error (fdefinition (list 'setf (gensym))))
  undefined-function)

(deftest fdefinition.error.6
  (classify-error (locally (fdefinition 10) t))
  type-error)

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

