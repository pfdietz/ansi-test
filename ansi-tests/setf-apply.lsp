;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Oct  6 22:11:00 2002
;;;; Contains: Tests of (SETF (APPLY ...) ...) forms

(in-package :cl-test)

;;;
;;; According to section 5.1.2.5 of the CL Spec,
;;; there are three kinds of (APPLY ...) forms that
;;; may appear in the first argument of a SETF form.
;;;

(deftest setf-apply.1
  (let ((x (vector 0 1 2 3 4 5)))
    (setf (apply #'aref x '(0)) 10)
    x)
  #(10 1 2 3 4 5))

(deftest setf-apply.2
  (let ((a (make-array '(2 2) :initial-contents '((0 0)(0 0)))))
    (setf (apply #'aref a 1 1 nil) 'a)
    (equalp a (make-array '(2 2) :initial-contents '((0 0)(0 a)))))
  t)

(deftest setf-apply.3
  (let ((bv (copy-seq #*0000000000)))
    (setf (apply #'bit bv 4 nil) 1)
    bv)
  #*0000100000)

(deftest setf-apply.4
  (let ((bv (copy-seq #*0000000000)))
    (setf (apply #'sbit bv 4 nil) 1)
    bv)
  #*0000100000)



