;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Oct  8 00:09:14 2002
;;;; Contains: Tests for FMAKUNBOUND

(in-package :cl-test)

(deftest fmakunbound.1
  (let ((g (gensym)))
    (and (not (fboundp g))
	 (setf (symbol-function g) #'car)
	 (fboundp g)
	 (values (eqt (check-values (fmakunbound g)) g)
		 (fboundp g))))
  t nil)

(deftest fmakunbound.2
  (let ((g (gensym)))
    (and (not (fboundp g))
	 (eval `(defun ,g () nil))
	 (fboundp g)
	 (values (eqt (check-values (fmakunbound g)) g)
		 (fboundp g))))
  t nil)

(deftest fmakunbound.3
  (let ((g (gensym)))
    (and (not (fboundp g))
	 (eval `(defmacro ,g () nil))
	 (fboundp g)
	 (values (eqt (check-values (fmakunbound g)) g)
		 (fboundp g))))
  t nil)

(deftest fmakunbound.4
  (let* ((g (gensym))
	 (n `(setf ,g)))
    (and (not (fboundp n))
	 (eval `(defun ,n () nil))
	 (fboundp n)
	 (values (equal (check-values (fmakunbound n)) n)
		 (fboundp n))))
  t nil)

(deftest fmakunbound.error.1
  (signals-error (fmakunbound 1) type-error)
  t)

(deftest fmakunbound.error.2
  (signals-error (fmakunbound #\a) type-error)
  t)

(deftest fmakunbound.error.3
  (signals-error (fmakunbound '(x)) type-error)
  t)

(deftest fmakunbound.error.4
  (signals-error (fmakunbound) program-error)
  t)

(deftest fmakunbound.error.5
  (signals-error (fmakunbound (gensym) nil) program-error)
  t)

(deftest fmakunbound.error.6
  (signals-error (locally (fmakunbound 1) t) type-error)
  t)

