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
	 (values (eq (fmakunbound g) g)
		 (fboundp g))))
  t nil)

(deftest fmakunbound.2
  (let ((g (gensym)))
    (and (not (fboundp g))
	 (eval `(defun ,g () nil))
	 (fboundp g)
	 (values (eq (fmakunbound g) g)
		 (fboundp g))))
  t nil)

(deftest fmakunbound.3
  (let ((g (gensym)))
    (and (not (fboundp g))
	 (eval `(defmacro ,g () nil))
	 (fboundp g)
	 (values (eq (fmakunbound g) g)
		 (fboundp g))))
  t nil)

(deftest fmakunbound.4
  (let* ((g (gensym))
	 (n `(setf ,g)))
    (and (not (fboundp n))
	 (eval `(defun ,n () nil))
	 (fboundp n)
	 (values (equal (fmakunbound n) n)
		 (fboundp n))))
  t nil)

(deftest fmakunbound.5
  (catch-type-error (fmakunbound 1))
  type-error)

(deftest fmakunbound.6
  (catch-type-error (fmakunbound #\a))
  type-error)

(deftest fmakunbound.7
  (catch-type-error (fmakunbound '(x)))
  type-error)

