;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 28 06:37:41 2005
;;;; Contains: Tests for nonstandard exceptional conditions in section 3

(in-package :ba-test)

(declaim (notinline compile-fails?))

;;; Utility functions

(defun compile-fails? (&rest args)
  (cl:handler-case
   (let ((vals (multiple-value-list (apply #'compile args))))
     (if (and (= (length vals) 3)
	      (cadr vals)
	      (caadr vals))
	 t
       (apply #'values nil vals)))
   (error () t)))

(defun function-name-p (x)
  (or (symbolp x)
      (and (consp x)
	   (eql (car x) 'setf)
	   (consp (cdr x))
	   (symbolp (cadr x))
	   (null (cddr x)))))

(defun symbol-or-function-p (x)
  (or (symbolp x)
      (and (consp x)
	   (eql (car x) 'function)
	   (consp (cdr x))
	   (null (cddr x))
	   (function-name-p (cadr x)))))

;;; Tests of COMPILE

(deftest compile.1
  (loop for x in *mini-universe*
	unless (or (function-name-p x)
		   (compile-fails? x))
	collect x)
  nil)

(deftest compile.2
  (compile-fails? nil)
  t)

(deftest compile.3
  (let ((sym (gensym)))
    (eval `(defun ,sym () nil))
    (loop for x in *mini-universe*
	  unless (or (functionp x) (and (consp x) (eql (car x) 'lambda))
		     (compile-fails? sym x))
	  collect x))
  nil)

(deftest compile.4
  (compile-fails? nil '(lambda))
  t)

(deftest compile.5
  (compile-fails? nil '(lambda x))
  t)

;;; EVAL-WHEN tests

(deftest eval-when.1
  (loop for x in *mini-universe*
	unless (or (listp x)
		   (eval `(signals-error (eval-when ,x nil) error)))
	collect x)
  nil)

;;; LOAD-TIME-VALUE

(deftest load-time-value.1
  (signals-error (load-time-value) error)
  t)

(deftest load-time-value.2
  (signals-error (load-time-value nil nil nil) error)
  t)

;;; QUOTE

(deftest quote.1
  (signals-error (quote) error)
  t)

(deftest quote.2
  (signals-error (quote . x) error)
  t)

(deftest quote.3
  (signals-error (quote t . x) error)
  t)

(deftest quote.4
  (signals-error (quote t x) error)
  t)

;;; COMPILER-MACRO-FUNCTION

(deftest compiler-macro-function.1
  (loop for x in *mini-universe*
	unless (or (function-name-p x)
		   (eval `(signals-error (compiler-macro-function ',x) error)))
	collect x)
  nil)

(deftest compiler-macro-function.2
  (loop for x in *mini-universe*
	unless (or (function-name-p x)
		   (eval `(signals-error
			   (setf (compiler-macro-function ',x) #'rplacd)
			   error)))
	collect x)
  nil)

;;; DEFINE-COMPILER-MACRO

(deftest define-compiler-macro.1
  (signals-error (define-compiler-macro) error)
  t)

(deftest define-compiler-macro.2
  (let ((sym (gensym)))
    (eval `(signals-error (define-compiler-macro ,sym) error)))
  t)

(deftest define-compiler-macro.3
  (signals-error (define-compiler-macro . foo) error)
  t)

(deftest define-compiler-macro.4
  (let ((sym (gensym)))
    (eval `(signals-error (define-compiler-macro ,sym () . foo) error)))
  t)

;;; DEFMACRO

(deftest defmacro.1
  (signals-error (defmacro) error)
  t)

(deftest defmacro.2
  (let ((sym (gensym)))
    (eval `(signals-error (defmacro ,sym) error)))
  t)

(deftest defmacro.3
  (signals-error (defmacro . foo) error)
  t)

(deftest defmacro.4
  (let ((sym (gensym)))
    (eval `(signals-error (defmacro ,sym () . foo) error)))
  t)

;;; MACRO-FUNCTION

(deftest macro-funtion.1
  (loop for x in *mini-universe*
	unless (or (symbolp x)
		   (eval `(signals-error (macro-function ',x) error)))
	collect x)
  nil)

(deftest macro-funtion.2
  (loop for x in *mini-universe*
	unless (or (symbolp x)
		   (eval `(signals-error (setf (macro-function ',x)
					       (macro-function 'car))
					 error)))
	collect x)
  nil)


;;; DEFINE-SYMBOL-MACRO

(deftest define-symbol-macro.1
  (let ((sym (gensym)))
    (eval `(signals-error (define-symbol-macro ,sym) error)))
  t)

(deftest define-symbol-macro.2
  (let ((sym (gensym)))
    (eval `(signals-error (define-symbol-macro ,sym t nil) error)))
  t)


(deftest define-symbol-macro.3
  (loop for x in *mini-universe*
	unless (or (symbolp x)
		   (eval `(signals-error (define-symbol-macro ',x) error)))
	collect x)
  nil)

;;; IGNORE

(deftest ignore.1
  (loop for x in *mini-universe*
	unless (or (symbol-or-function-p x)
		   (eval `(signals-error (locally (declare (ignore ,x)) nil)
					 error)))
	collect x)
  nil)

(deftest ignore.2
  (signals-error (locally (declare (ignore . foo)) nil) error)
  t)

;;; IGNORABLE

(deftest ignorable.1
  (loop for x in *mini-universe*
	unless (or (symbol-or-function-p x)
		   (eval `(signals-error
			   (locally (declare (ignorable ,x)) nil)
			   error)))
	collect x)
  nil)

(deftest ignorable.2
  (signals-error (locally (declare (ignorable . foo)) nil) error)
  t)

;;; DYNAMIC-EXTENT

(deftest dynamic-extent.1
  (loop for x in *mini-universe*
	unless (or (symbol-or-function-p x)
		   (eval `(signals-error
			   (locally (declare (dynamic-extent ,x)) nil)
			   error)))
	collect x)
  nil)

(deftest dynamic-extent.2
  (signals-error (locally (declare (dynamic-extent . foo)) nil) error)
  t)

;;; TYPE declarations
;;; Test that violation of the type declarations is detected, and
;;; leads to an error in safe code.

(deftest type.1
  (loop for x in *mini-universe*
	for tp = (type-of x)
	for lambda-form = `(lambda (y) (declare (optimize safety)
						(type (not ,tp) y)) y)
	for fn = (progn (print lambda-form)
			(eval `(function ,lambda-form)))
	unless (eval `(signals-error (funcall ',fn ',x) error))
	collect x)
  nil)
