;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Oct 19 11:42:14 2002
;;;; Contains: Tests of DEFINE-MODIFY-MACRO

(in-package :cl-test)

(deftest define-modify-macro.1
  (let ((u '(p q r)) v)
    (values
     (define-modify-macro dmm1-appendf (&rest args) 
       append "Append lists onto a list")
     (setq v u)
     (dmm1-appendf u '(a b c d))
     (dmm1-appendf u ())
     (dmm1-appendf u '(e f g))
     u
     v))
  dmm1-appendf
  (p q r)
  (p q r a b c d)
  (p q r a b c d)
  (p q r a b c d e f g)
  (p q r a b c d e f g)
  (p q r))

(deftest define-modify-macro.2
  (let ((i 10))
    (values
     (define-modify-macro new-incf (&optional (delta 1)) +)
     (new-incf i)
     (new-incf i 100)
     i))
  new-incf
  11
  111
  111)

(deftest define-modify-macro.3
  (let ((a (vector 0 0 0 0 0))
	(i 1))
    (values
     (define-modify-macro new-incf (&optional (delta 1)) +)
     (new-incf (aref a (incf i)))
     a
     i))
  new-incf
  1
  #(0 0 1 0 0)
  2)

(deftest define-modify-macro.4
  (let ((a (vector 0 0 0 0 0))
	(i 1))
    (values
     (define-modify-macro new-incf (&optional (delta 1)) +)
     (new-incf (aref a (incf i)) (incf i))
     a
     i))
  new-incf
  3
  #(0 0 3 0 0)
  3)


  