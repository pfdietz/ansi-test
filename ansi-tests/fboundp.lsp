;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Oct  7 22:37:22 2002
;;;; Contains: Tests of FBOUNDP

(in-package :cl-test)

(deftest fboundp.1
  (not (fboundp 'car))
  nil)

(deftest fboundp.2
  (not (fboundp 'cdr))
  nil)

(deftest fboundp.3
  (not (fboundp 'defun))  ; a macro
  nil)

(deftest fboundp.4
  ;; fresh symbols are not fbound
  (let ((g (gensym))) (fboundp g))
  nil)

(defun fboundp-5-fn (x) x)
(deftest fboundp.5
  (not (fboundp 'fboundp-5-fn))
  nil)

(eval-when (eval compile)
  (ignore-errors
    (defun (setf fboundp-6-accessor) (y x) (setf (car x) y))))

(deftest fboundp.6
  (not (fboundp '(setf fboundp-6-accessor)))
  nil)

(deftest fboundp.7
  (let ((g (gensym))) (fboundp (list 'setf g)))
  nil)

(deftest fboundp.8
  (classify-error (fboundp 1))
  type-error)

(deftest fboundp.9
  (classify-error (fboundp #\a))
  type-error)

(deftest fboundp.10
  (classify-error (fboundp '(foo)))
  type-error)






  
