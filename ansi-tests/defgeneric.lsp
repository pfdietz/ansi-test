;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 17 20:55:50 2003
;;;; Contains: Tests of DEFGENERIC

(in-package :cl-test)

;;; Various error cases

(defun defgeneric-testfn-01 (x) x)

(deftest defgeneric.error.1
  ;; Cannot make ordinary functions generic
  (let* ((name 'defgeneric-testfn-01)
	 (fn (symbol-function name)))
    (if (not (typep fn 'generic-function))
	(handler-case
	 (progn (eval `(defgeneric ,name ())) :bad)
	 (program-error () :good))
      :good))
  :good)

(defmacro defgeneric-testmacro-02 (x) x)

(deftest defgeneric.error.2
  ;; Cannot make macros generic
  (let* ((name 'defgeneric-testmacro-02))
    (handler-case
     (progn (eval `(defgeneric ,name ())) :bad)
     (program-error () :good)))
  :good)

(deftest defgeneric.error.3
  ;; Cannot make special operators generic
  (loop for name in *cl-special-operator-symbols*
	for result =
	(handler-case
	 (progn (eval `(defgeneric ,name ())) t)
	 (program-error () nil))
	when result collect name)
  nil)

(deftest defgeneric.error.4
  (classify-error (defgeneric defgeneric-error-fn.4 (x y)
		    (:argument-precedence-order x y x)))
  program-error)

(deftest defgeneric.error.5
  (classify-error (defgeneric defgeneric-error-fn.5 (x)
		    (:documentation "some documentation")
		    (:documentation "illegally repeated documentation")))
  program-error)

(deftest defgeneric.error.6
  (classify-error (defgeneric defgeneric-error-fn.6 (x)
		    (unknown-option nil)))
  program-error)

(deftest defgeneric.error.7
  (handler-case
   (progn
     (eval '(defgeneric defgeneric-error-fn.7 (x y)
	      (:method ((x t)) x)))
     :bad)
   (error () :good))
  :good)

(deftest defgeneric.error.8
  (classify-error (defgeneric defgeneric-error-fn.8 (x y)
		    (:argument-precedence-order x)))
  program-error)