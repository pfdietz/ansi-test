;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 10 14:39:01 2003
;;;; Contains: Tests for SLOT-MAKUNBOUND

(in-package :cl-test)

;;; This function is heavily tested in other files as well

(defclass slot-makunbound-class-01 ()
  (a
   (b :allocation :instance)
   (c :allocation :class)
   (d :type fixnum)
   (e :type t)
   (f :type cons)))

(deftest slot-makunbound.1
  (loop for slot-name in '(a b c d e)
	unless
	(let ((obj (allocate-instance (find-class 'slot-makunbound-class-01))))
	  (and
	   (equalt (multiple-value-list (slot-makunbound obj slot-name))
		   (list obj))
	   (not (slot-boundp obj slot-name))))
	collect slot-name)
  nil)

(deftest slot-makunbound.2
  (loop for slot-name in '(a b c d e)
	for slot-value in '(t t t 10 t '(a))
	unless
	(let ((obj (allocate-instance (find-class 'slot-makunbound-class-01))))
	  (setf (slot-value obj slot-name) slot-value)
	  (and
	   (equalt (multiple-value-list (slot-makunbound obj slot-name))
		   (list obj))
	   (not (slot-boundp obj slot-name))))
	collect slot-name)
  nil)



;;; Error cases

(deftest slot-makunbound.error.1
  (classify-error (slot-makunbound))
  program-error)

(deftest slot-makunbound.error.2
  (classify-error (slot-makunbound (make-instance 'slot-makunbound-class-01)))
  program-error)

(deftest slot-makunbound.error.3
  (classify-error (slot-makunbound (make-instance 'slot-makunbound-class-01)
				   'a nil))
  program-error)

(deftest slot-makunbound.error.4
  (let ((built-in-class (find-class 'built-in-class)))
    (loop for e in *mini-universe*
	  for class = (class-of e)
	  when (and (eq (class-of class) built-in-class)
		    (handler-case (progn (slot-makunbound e 'foo) t)
				  (error () nil)))
	  collect e))
  nil)

