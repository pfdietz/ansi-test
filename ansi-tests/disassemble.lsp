;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun May 18 20:47:58 2003
;;;; Contains: Tests of DISASSEMBLE

(in-package :cl-test)

(defun disassemble-it (fn)
  (let (val)
    (values
     (notnot
      (stringp
       (with-output-to-string (*standard-output*)
			      (setf val (disassemble fn)))))
     val)))

(deftest disassemble.1
  (disassemble-it 'car)
  t nil)

(deftest disassemble.2
  (disassemble-it (symbol-function 'car))
  t nil)

(deftest disassemble.3
  (disassemble-it '(lambda (x y) (cons y x)))
  t nil)

(deftest disassemble.4
  (disassemble-it (eval '(function (lambda (x y) (cons y z)))))
  t nil)

(deftest disassemble.5
  (disassemble-it
   (funcall (compile nil '(lambda () (let ((x 0)) #'(lambda () (incf x)))))))
  t nil)

(deftest disassemble.6
  (let ((name 'disassemble.fn.1))
    (fmakunbound name)
    (eval `(defun ,name (x) x))
    (disassemble-it name))
  t nil)

(deftest disassemble.7
  (let ((name 'disassemble.fn.2))
    (fmakunbound name)
    (eval `(defun ,name (x) x))
    (compile name)
    (disassemble-it name))
  t nil)
