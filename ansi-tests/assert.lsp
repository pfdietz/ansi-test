;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan 28 06:48:19 2003
;;;; Contains: Tests of ASSERT

(in-package :cl-test)

(deftest assert.1
  (assert t)
  nil)

(deftest assert.2
  (assert t ())
  nil)

;;; I am assuming that when no places are given to ASSERT,
;;; it doesn't invoke any interactive handler.

(deftest assert.3
  (let ((x nil))
    (handler-bind
     ((error #'(lambda (c)
		 (setq x 17)
		 (let ((r (find-restart 'continue c)))
		   (when r (invoke-restart r))))))
     (progn
       (assert x)
       x)))
  17)

;;; I don't yet know how to test the interactive version of ASSERT
;;; that is normally invoked when places are given.

;;; Tests of the syntax (at least)

(deftest assert.4
  (let (x)
    (assert t (x)))
  nil)

(deftest assert.5
  (let ((x (cons 'a 'b)))
    (assert t ((car x) (cdr x))))
  nil)

(deftest assert.6
  (let ((x (vector 'a 'b 'c)))
    (assert t ((aref x 0) (aref x 1) (aref x 2))
	    "Vector x has value: ~A." x))
  nil)


