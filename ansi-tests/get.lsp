;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jul 13 07:01:47 2004
;;;; Contains: Tests of GET

(in-package :cl-test)

(deftest get.1
  (let ((sym (gensym))) (get sym :foo))
  nil)

(deftest get.2
  (let ((sym (gensym))) (get sym :foo :bar))
  :bar)

(deftest get.3
  (let ((sym (gensym))) (get sym :foo (values :bar nil)))
  :bar)

(deftest get.4
  (let ((sym (gensym)))
    (setf (symbol-plist sym) (list :foo 1 :bar 2 :foo 3))
    (values (get sym :foo) (get sym :bar)))
  1 2)

(deftest get.5
  (let ((evaluated nil)
	(sym (gensym)))
    (assert (equal (multiple-value-list (setf (get sym :foo) 1))
		   '(1)))
    (values
     (get sym :foo (progn (setf evaluated t) nil))
     evaluated))
  1 t)

(deftest get.6
  (let ((evaluated nil)
	(sym (gensym)))
    (assert (equal (multiple-value-list
		    (setf (get sym :foo
			       (progn (setf evaluated t) nil))
			  1))
		   '(1)))
    (values
     (get sym :foo)
     evaluated))
  1 t)

;;; Error tests

(deftest get.error.1
  (signals-error (get) program-error)
  t)

(deftest get.error.2
  (signals-error (get nil) program-error)
  t)

(deftest get.error.3
  (signals-error (get nil nil nil nil) program-error)
  t)

(deftest get.error.4
  (loop for x in *mini-universe*
	for form = `(signals-error (get ',x :foo) type-error)
	unless (or (symbolp x) (eval form))
	collect x)
  nil)

(deftest get.error.5
  (loop for x in *mini-universe*
	for form = `(signals-error (setf (get ',x :foo) nil) type-error)
	unless (or (symbolp x) (eval form))
	collect x)
  nil)



 