;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 12:33:02 2003
;;;; Contains: Tests of DEFINE-COMPILER-MACRO

(in-package :cl-test)

;;; Need to add non-error tests

(deftest define-compiler-macro.error.1
  (signals-error (funcall (macro-function 'define-compiler-macro))
		 program-error)
  t)

(deftest define-compiler-macro.error.2
  (signals-error (funcall (macro-function 'define-compiler-macro)
			   '(definee-compiler-macro nonexistent-function ()))
		 program-error)
  t)

(deftest define-compiler-macro.error.3
  (signals-error (funcall (macro-function 'define-compiler-macro)
			   '(definee-compiler-macro nonexistent-function ())
			   nil nil)
		 program-error)
  t)

;;; Non-error tests

(deftest define-compiler-macro.1
  (let* ((sym (gensym))
	 (macro-def-form
	  `(define-compiler-macro ,sym (x y)
	     (declare (special *x*))
	     (setf *x* t)
	     `(+ ,x ,y 1)))
	 (fun-def-form
	  `(defun ,sym (x y) (+ x y 1))))
    (values
     (equalt (list sym) (multiple-value-list (eval fun-def-form)))
     (equalt (list sym) (multiple-value-list (eval macro-def-form)))
     (notnot (typep (compiler-macro-function sym) 'function))
     (eval `(,sym 6 19))
     (let ((fn (compile nil `(lambda (a b) (,sym a b)))))
       (let ((*x* nil))
	 (declare (special *x*))
	 (list (funcall fn 12 123) *x*)))))
  t t t 26 (136 nil))

(deftest define-compiler-macro.2
  (let* ((sym (gensym))
	 (macro-def-form
	  `(define-compiler-macro ,sym (&whole form &rest args)
	     (declare (special *x*) (ignore args))
	     (setf *x* t)
	     (return-from ,sym form)))
	 (fun-def-form
	  `(defun ,sym (x) x)))
    (values
     (equalt (list sym) (multiple-value-list (eval fun-def-form)))
     (equalt (list sym) (multiple-value-list (eval macro-def-form)))
     (notnot (typep (compiler-macro-function sym) 'function))
     (eval `(,sym 'a))
     (let ((fn (compile nil `(lambda (a) (,sym a)))))
       (let ((*x* nil))
	 (declare (special *x*))
	 (list (funcall fn 'b) *x*)))))
  t t t a (b nil))

(deftest define-compiler-macro.3
  (let* ((sym (gensym))
	 (macro-def-form
	  `(define-compiler-macro ,sym (&whole form &rest args)
	     (declare (special *x*) (ignore args))
	     (setf *x* t)
	     (return-from ,sym form)))
	 (ordinary-macro-def-form
	  `(defmacro ,sym (x) x)))
    (values
     (equalt (list sym) (multiple-value-list (eval ordinary-macro-def-form)))
     (equalt (list sym) (multiple-value-list (eval macro-def-form)))
     (notnot (typep (compiler-macro-function sym) 'function))
     (eval `(,sym 'a))
     (let ((fn (compile nil `(lambda (a) (,sym a)))))
       (let ((*x* nil))
	 (declare (special *x*))
	 (list (funcall fn 'b) *x*)))))
  t t t a (b nil))
