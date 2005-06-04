;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Jun  3 22:17:34 2005
;;;; Contains: Tests of MACRO-FUNCTION

(in-package :cl-test)

(deftest macro-function.1
  (loop for n in *cl-macro-symbols*
	unless (macro-function n)
	collect n)
  nil)

(deftest macro-function.2
  (loop for n in *cl-macro-symbols*
	unless (macro-function n nil)
	collect n)
  nil)

(deftest macro-function.3
  (loop for n in *cl-macro-symbols*
	unless (eval `(macrolet ((%m (s &environment env)
				     (list 'quote
					   (macro-function s env))))
			(%m ,n)))
	collect n)
  nil)

(deftest macro-function.4
  (macro-function (gensym))
  nil)

(deftest macro-function.5
  (remove-if-not #'macro-function *cl-function-symbols*)
  nil)

(deftest macro-function.6
  (remove-if-not #'macro-function *cl-accessor-symbols*)
  nil)

(deftest macro-function.7
  (let ((fn
	 (macrolet ((%m () 16))
	   (macrolet ((%n (&environment env)
			  (list 'quote (macro-function '%m env))))
	     (%n)))))
    (values
     (notnot (functionp fn))
     (funcall fn '(%m) nil)))
  t 16)

(deftest macro-function.8
  (let ((sym (gensym)))
    (setf (macro-function sym) (macro-function 'pop))
    (eval `(let ((x '(a b c)))
	     (values 
	      (,sym x)
	      x))))
  a (b c))

(deftest macro-function.9
  (let ((sym (gensym)))
    (setf (macro-function sym nil) (macro-function 'pop))
    (eval `(let ((x '(a b c)))
	     (values 
	      (,sym x)
	      x))))
  a (b c))

(deftest macro-function.10
  (let ((sym (gensym)))
    (eval `(defun ,sym (x) :bad))
    (setf (macro-function sym) (macro-function 'pop))
    (eval `(let ((x '(a b c)))
	     (values 
	      (,sym x)
	      x))))
  a (b c))

(deftest macro-function.11
  (let ((fn
	 (flet ((%m () 16))
	   (macrolet ((%n (&environment env)
			  (list 'quote (macro-function '%m env))))
	     (%n)))))
     fn)
  nil)

;;; Error tests

(deftest macro-function.error.1
  (signals-error (macro-function) program-error)
  t)

(deftest macro-function.error.2
  (signals-error (macro-function 'pop nil nil) program-error)
  t)

