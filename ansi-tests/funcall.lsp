;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Oct  9 21:45:07 2002
;;;; Contains: Tests of FUNCALL

(in-package :cl-test)

(deftest funcall.1
  (let ((fn #'cons))
    (funcall fn 'a 'b))
  (a . b))

(deftest funcall.2
  (funcall (symbol-function 'cons) 'a 'b)
  (a . b))

(deftest funcall.3
  (let ((fn 'cons))
    (funcall fn 'a 'b))
  (a . b))

(deftest funcall.4
  (funcall 'cons 'a 'b)
  (a . b))

(deftest funcall.5
  (let ((fn #'+))
    (funcall fn 1 2 3 4))
  10)

(deftest funcall.6
  (funcall #'(lambda (x y) (cons x y)) 'a 'b)
  (a . b))

(deftest funcall.7
  (flet ((cons (x y) (list y x)))
    (values (funcall 'cons 1 2)
	    (funcall #'cons 1 2)))
  (1 . 2)
  (2 1))

;;; FUNCALL should throw an UNDEFINED-FUNCTION condition when
;;; called on a symbol with a global definition as a special
;;; operator
(deftest funcall.error.1
  (classify-error (funcall 'quote 1))
  undefined-function)

(deftest funcall.error.2
  (classify-error (funcall 'progn 1))
  undefined-function)

;;; FUNCALL should throw an UNDEFINED-FUNCTION condition when
;;; called on a symbol with a global definition as a macro
(deftest funcall.error.3
  (classify-error (funcall 'defconstant '(defconstant x 10)))
  undefined-function)

(deftest funcall.error.4
  (classify-error (funcall))
  program-error)

