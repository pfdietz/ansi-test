;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 17 09:16:20 2003
;;;; Contains: Tests of MAKE-LOAD-FORM

(in-package :cl-test)

;;; These tests are just of MAKE-LOAD-FORM itself; tests of file compilation
;;; that depend on MAKE-LOAD-FORM will be found elsewhere.

(defclass make-load-form-class-01 () (a b c))

(deftest make-load-form.1
  (handler-case
   (progn (make-load-form (make-instance 'make-load-form-class-01))
	  :bad)
   (error () :good))
  :good)

(defstruct make-load-form-struct-02 a b c)

(deftest make-load-form.2
  (handler-case
   (progn (make-load-form (make-make-load-form-struct-02)) :bad)
   (error () :good))
  :good)

(define-condition make-load-form-condition-03 () (a b c))

(deftest make-load-form.3
  (handler-case
   (progn (make-load-form (make-condition 'make-load-form-condition-03) :bad))
   (error () :good))
  :good)

;;; Make sure these errors are due to the method, not due to lack of
;;; methods

(deftest make-load-form.4
  (let* ((obj (make-instance 'make-load-form-class-01))
	 (fun #'make-load-form)
	 (methods (compute-applicable-methods fun (list obj))))
     (notnot-mv methods))
  t)

(deftest make-load-form.5
  (let* ((obj (make-make-load-form-struct-02))
	 (fun #'make-load-form)
	 (methods (compute-applicable-methods fun (list obj))))
    (notnot-mv methods))
  t)

(deftest make-load-form.6
  (let* ((obj (make-condition 'make-load-form-condition-03))
	 (fun #'make-load-form)
	 (methods (compute-applicable-methods fun (list obj))))
    (notnot-mv methods))
  t)

(deftest make-load-form.7
  (let* ((obj (make-instance 'make-load-form-class-01))
	 (fun #'make-load-form)
	 (methods (compute-applicable-methods fun (list obj nil))))
    (notnot-mv methods))
  t)

(deftest make-load-form.8
  (let* ((obj (make-make-load-form-struct-02))
	 (fun #'make-load-form)
	 (methods (compute-applicable-methods fun (list obj nil))))
    (notnot-mv methods))
  t)

(deftest make-load-form.9
  (let* ((obj (make-condition 'make-load-form-condition-03))
	 (fun #'make-load-form)
	 (methods (compute-applicable-methods fun (list obj nil))))
    (notnot-mv methods))
  t)
  
(deftest make-load-form.10
  (macrolet
      ((%m (&environment env)
	   (let* ((obj (make-instance 'make-load-form-class-01))
		  (fun #'make-load-form)
		  (methods (compute-applicable-methods fun (list obj env))))
	     (notnot-mv methods))))
    (%m))
  t)

(deftest make-load-form.11
  (macrolet
      ((%m (&environment env)
	   (let* ((obj (make-make-load-form-struct-02))
		  (fun #'make-load-form)
		  (methods (compute-applicable-methods fun (list obj env))))
	     (notnot-mv methods))))
    (%m))
  t)

(deftest make-load-form.12
  (macrolet
      ((%m (&environment env)
	   (let* ((obj (make-condition 'make-load-form-condition-03))
		  (fun #'make-load-form)
		  (methods (compute-applicable-methods fun (list obj env))))
	     (notnot-mv methods))))
    (%m))
  t)
