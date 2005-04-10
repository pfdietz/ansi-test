;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Dec 14 07:30:01 2004
;;;; Contains: Tests of DOCUMENTATION

(in-package :cl-test)

(deftest documentation.1
  (let* ((sym (gensym)))
    (eval `(defun ,sym () nil))
    (documentation (symbol-function sym) t))
  nil)

(deftest documentation.2
  (let* ((sym (gensym)))
    (eval `(defun ,sym () nil))
    (let ((fn (symbol-function sym))
	  (doc "FOO"))
      (multiple-value-prog1
       (setf (documentation fn t) "FOO")
       (assert (or (null (documentation fn t))
		   (equal doc (documentation fn t)))))))
  "FOO")

(deftest documentation.3
  (let* ((sym (gensym)))
    (eval `(defun ,sym () nil))
    (documentation (symbol-function sym) 'function))
  nil)

(deftest documentation.4
  (let* ((sym (gensym)))
    (eval `(defun ,sym () nil))
    (let ((fn (symbol-function sym))
	  (doc "FOO"))
      (multiple-value-prog1
       (setf (documentation fn 'function) "FOO")
       (assert (or (null (documentation fn 'function))
		   (equal doc (documentation fn 'function)))))))
  "FOO")



	



