;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Feb 16 23:40:32 2003
;;;; Contains: Tests of DEFUN

(in-package :cl-test)

#|
(deftest defun.error.1
  (classify-error (defun))
  program-error)

(deftest defun.error.2
  (classify-error (defun ignored-defun-name))
  program-error)
|#

;;; Tests for implicit blocks

(defun defun-test-fun-1 ()
  (return-from defun-test-fun-1 'good))

(deftest defun.1
  (defun-test-fun-1)
  good)

(defun defun-test-fun-2 ()
  (return-from defun-test-fun-2 (values)))

(deftest defun.2
  (defun-test-fun-2))

(defun defun-test-fun-3 ()
  (return-from defun-test-fun-3 (values 'a 'b 'c 'd 'e 'f)))

(deftest defun.3
  (defun-test-fun-3)
  a b c d e f)

(defun defun-test-fun-4 (x)
  (car x))

(eval-when (load eval compile)
  (ignore-errors
    (defun (setf defun-test-fun-4) (newval x)
      (return-from defun-test-fun-4 (setf (car x) newval)))))

(deftest defun.4
  (let ((x (list 'a 'b)))
    (values
     (setf (defun-test-fun-4 x) 'c)
     x))
  c
  (c b))




