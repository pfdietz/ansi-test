;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Oct  7 07:34:29 2002
;;;; Contains: Tests for type FUNCTION and the special form FUNCTION

(in-package :cl-test)

;;;
;;; Note! There are significant incompatibilities between CLTL1 and ANSI CL
;;; in the meaning of FUNCTION and FUNCTIONP.
;;;

(deftest function.1
  (typep nil 'function)
  nil)

;;; The next test demonstrates an incompatibility between CLtL1 and ANSI CL.
;;; In ANSI CL, symbols are no longer of type FUNCTION.
(deftest function.2
  (typep 'identity 'function)
  nil)

(deftest function.3
  (not-mv (typep #'identity 'function))
  nil)

(deftest function.4
  (loop for x in *cl-symbol-names*
	for s = (find-symbol x "CL")
	for f = (and (fboundp s)
		     (symbol-function s)
		     (not (special-operator-p s))
		     (not (macro-function s))
		     (symbol-function s))
	unless (or (null f)
		   (typep f 'function))
	collect x)
  nil)

(deftest function.5
  (typep '(setf car) 'function)
  nil)

;;; The next test demonstrates an incompatibility between CLtL1 and ANSI CL.
;;; In ANSI CL, lambda forms are no longer of type FUNCTION.
(deftest function.6
  (typep '(lambda (x) x) 'function)
  nil)

(eval-when (eval compile)
  (ignore-errors
    (defun (setf function-7-accessor) (y x) (setf (car x) y) y)))

(deftest function.7
  (not-mv (typep #'(setf function-7-accessor) 'function))
  nil)

(deftest function.8
  (not-mv (typep #'(lambda (x) x) 'function))
  nil)

(deftest function.9
  (not-mv (typep (compile nil '(lambda (x) x)) 'function))
  nil)

;;; The next test demonstrates an incompatibility between CLtL1 and ANSI CL.
;;; In ANSI CL, symbols and cons can no longer also be of type FUNCTION.
(deftest function.10
  (loop for x in *universe*
	when (and (or (numberp x) (characterp x)
		      (symbolp x) (consp x)
		      (typep x 'array))
		  (typep x 'function))
	collect x)
  nil)

(deftest function.11
  (flet ((%f () nil)) (typep '%f 'function))
  nil)

(deftest function.12
  (flet ((%f () nil)) (not-mv (typep #'%f 'function)))
  nil)

(deftest function.13
  (labels ((%f () nil)) (not-mv (typep #'%f 'function)))
  nil)

;;; "If name is a function name, the functional definition of that
;;; name is that established by the innermost lexically enclosing flet,
;;; labels, or macrolet form, if there is one." (page for FUNCTION, sec. 5.3)
;;;            ^^^^^^^^
;;;(deftest function.14
;;;  (macrolet ((%f () nil)) (not-mv (typep #'%f 'function)))
;;;  nil)
