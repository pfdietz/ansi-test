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

(report-and-ignore-errors
 (defun (setf fboundp-6-accessor) (y x) (setf (car x) y)))

(deftest fboundp.6
  (not-mv (fboundp '(setf fboundp-6-accessor)))
  nil)

(deftest fboundp.7
  (let ((g (gensym))) (fboundp (list 'setf g)))
  nil)

;;; See 11.1.2.1.1
(deftest fboundp.8
  (loop for x in *cl-non-function-macro-special-operator-symbols*
	when (and (fboundp x) (not (eq x 'ed)))
	collect x)
  nil)

(deftest fboundp.order.1
  (let ((i 0))
    (values (notnot (fboundp (progn (incf i) 'car))) i))
  t 1)

(deftest fboundp.error.1
  (signals-error (fboundp 1) type-error)
  t)

(deftest fboundp.error.2
  (signals-error (fboundp #\a) type-error)
  t)

(deftest fboundp.error.3
  (signals-error (fboundp '(foo)) type-error)
  t)

(deftest fboundp.error.4
  (signals-error (fboundp) program-error)
  t)

(deftest fboundp.error.5
  (signals-error (fboundp 'cons nil) program-error)
  t)

(deftest fboundp.error.6
  (signals-error (locally (fboundp 1) t) type-error)
  t)
