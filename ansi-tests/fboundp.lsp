;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Oct  7 22:37:22 2002
;;;; Contains: Tests of FBOUNDP

(in-package :cl-test)

(deftest fboundp.1
  (not-mv (fboundp 'car))
  nil)

(deftest fboundp.2
  (not-mv (fboundp 'cdr))
  nil)

(deftest fboundp.3
  (not-mv (fboundp 'defun))  ; a macro
  nil)

(deftest fboundp.4
  ;; fresh symbols are not fbound
  (let ((g (gensym))) (fboundp g))
  nil)

(defun fboundp-5-fn (x) x)
(deftest fboundp.5
  (not-mv (fboundp 'fboundp-5-fn))
  nil)

(eval-when (eval compile)
  (ignore-errors
    (defun (setf fboundp-6-accessor) (y x) (setf (car x) y))))

(deftest fboundp.6
  (not-mv (fboundp '(setf fboundp-6-accessor)))
  nil)

(deftest fboundp.7
  (let ((g (gensym))) (fboundp (list 'setf g)))
  nil)

(deftest fboundp.order.1
  (let ((i 0))
    (values (notnot (fboundp (progn (incf i) 'car))) i))
  t 1)

(deftest fboundp.error.1
  (classify-error (fboundp 1))
  type-error)

(deftest fboundp.error.2
  (classify-error (fboundp #\a))
  type-error)

(deftest fboundp.error.3
  (classify-error (fboundp '(foo)))
  type-error)

(deftest fboundp.error.4
  (classify-error (fboundp))
  program-error)

(deftest fboundp.error.5
  (classify-error (fboundp 'cons nil))
  program-error)

(deftest fboundp.error.6
  (classify-error (locally (fboundp 1) t))
  type-error)
