;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 22:05:34 2003
;;;; Contains: Tests of PUSH

(in-package :cl-test)

;;; See also places.lsp

(deftest push.1
  (let ((x nil))
    (push 'a x))
  (a))

(deftest push.2
  (let ((x 'b))
    (push 'a x)
    (push 'c x))
  (c a . b))

(deftest push.3
  (let ((x (copy-tree '(a))))
    (push x x)
    (and
     (eqt (car x) (cdr x))
     x))
  ((a) a))

(deftest push.order.1
  (let ((x (list nil)) (i 0) a b)
    (values
     (push (progn (setf a (incf i)) 'z)
	   (car (progn (setf b (incf i)) x)))
     x
     i a b))
  (z) ((z)) 2 1 2)

;;; Need to add push vs. various accessors
