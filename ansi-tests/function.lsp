;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Oct  7 07:34:29 2002
;;;; Contains: Tests for type FUNCTION

(in-package :cl-test)

;;;
;;; Note! There are significant incompatibilities between CLTL1 and ANSI CL
;;; in the meaning of FUNCTION and FUNCTIONP.
;;;

(deftest function.1
  (typep nil 'function)
  nil)

(deftest function.2
  (typep 'identity 'function)
  nil)

(deftest function.3
  (not (typep #'identity 'function))
  nil)

(deftest function.4
  (loop for x in *cl-symbol-names*
	for s = (find-symbol x "CL")
	for f = (and (fboundp s) (symbol-function s))
	always (or (null f)
		   (typep f 'function)
		   ))
  t)

(deftest function.5
  (typep '(setf car) 'function)
  nil)

(deftest function.6
  (typep '(lambda (x) x) 'function)
  nil)

(eval-when (eval compile)
  (ignore-errors
    (defun (setf function-7-accessor) (y x) (setf (car x) y) y)))

(deftest function.7
  (not (typep #'(setf function-7-accessor) 'function))
  nil)

(deftest function.8
  (not (typep #'(lambda (x) x) 'function))
  nil)

(deftest function.9
  (not (typep (compile nil '(lambda (x) x)) 'function))
  nil)

(deftest function.10
  (loop for x in *universe*
	never
	(and (or (numberp x) (characterp x) (symbolp x) (consp x)
		 (typep x 'array))
	     (typep x 'function)))
  t)

(deftest function.11
  (flet ((%f () nil)) (typep '%f 'function))
  nil)

(deftest function.12
  (flet ((%f () nil)) (not (typep #'%f 'function)))
  nil)