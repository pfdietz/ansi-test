;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Oct  7 06:39:21 2002
;;;; Contains: Tests for FUNCTIONP

(in-package :cl-test)

;;;
;;; Note!  FUNCTIONP and FUNCTION behave differently in ANSI CL than
;;; in CLTL1.  In particular, symbols and various lists are no longer
;;; in the class FUNCTION in ANSI CL.
;;;

(deftest functionp.1
  (functionp nil)
  nil)

(deftest functionp.2
  (functionp 'identity)
  nil)

(deftest functionp.3
  (not (functionp #'identity))
  nil)

(deftest functionp.4
  (loop for x in *cl-symbol-names*
	for s = (find-symbol x "CL")
	for f = (and (fboundp s) (symbol-function s))
	always (or (null f)
		   (functionp f)))
  t)

(deftest functionp.5
  (functionp '(setf car))
  nil)

(deftest functionp.6
  (functionp '(lambda (x) x))
  nil)

(eval-when (eval compile)
  (ignore-errors
    (defun (setf functionp-7-accessor) (y x) (setf (car x) y) y)))

(deftest functionp.7
  (not (functionp #'(setf functionp-7-accessor)))
  nil)

(deftest functionp.8
  (not (functionp #'(lambda (x) x)))
  nil)

(deftest functionp.9
  (not (functionp (compile nil '(lambda (x) x))))
  nil)

(deftest functionp.10
  (loop for x in *universe*
	never
	(and (or (numberp x) (characterp x) (symbolp x) (consp x)
		 (typep x 'array))
	     (functionp x)))
  t)

(deftest functionp.11
  (flet ((%f () nil)) (functionp '%f))
  nil)

(deftest functionp.12
  (flet ((%f () nil)) (not (functionp #'%f)))
  nil)


  
