;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jul 13 07:55:05 2004
;;;; Contains: Add tests for MAKUNBOUND

(in-package :cl-test)

(deftest makunbound.1
  (let ((sym (gensym)))
    (values
     (boundp sym)
     (equalt (multiple-value-list (makunbound sym)) (list sym))
     (boundp sym)
     (setf (symbol-value sym) nil)
     (notnot (boundp sym))
     (equalt (multiple-value-list (makunbound sym)) (list sym))
     (boundp sym)))
  nil t nil nil t t nil)

(deftest makunbound.2
  (let ((sym (gensym)))
    (values
     (boundp sym)
     (setf (symbol-value sym) :foo)
     (equalt (multiple-value-list (makunbound sym)) (list sym))
     (boundp sym)
     (handler-case (symbol-value sym)
		   (unbound-variable () :good))))
  nil :foo t nil :good)

;;; Error cases

(deftest makunbound.error.1
  (signals-error (makunbound) program-error)
  t)

(deftest makunbound.error.2
  (signals-error (makunbound (gensym) nil) program-error)
  t)

(deftest makunbound.error.3
  (loop for x in *mini-universe*
	for form = `(signals-error (makunbound ',x) type-error)
	unless (or (symbolp x) (eval form))
	collect x)
  nil)

