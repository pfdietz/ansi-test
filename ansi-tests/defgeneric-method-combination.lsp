;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 24 21:31:55 2003
;;;; Contains: Tests of DEFGENERIC with :method-combination

(in-package :cl-test)

(declaim (special *x*))

(deftest defgeneric-method-combination.1
  (let ((*x* nil)
	(fn
	 (eval '(defgeneric dg-mc.fun.1 (x)
		  (:method-combination +)
		  (:method + ((x integer)) (car (push 8 *x*)))
		  (:method + ((x rational)) (car (push 4 *x*)))
		  (:method + ((x number)) (car (push 2 *x*)))
		  (:method + ((x t)) (car (push 1 *x*)))))))
    (flet ((%f (y)
	       (let ((*x* nil))
		 (list (funcall fn y) *x*))))
    (values (%f 1) (%f 2/3) (%f 1.54) (%f 'a))))
  (15 (1 2 4 8)) (7 (1 2 4)) (3 (1 2)) (1 (1)))

(deftest defgeneric-method-combination.2
  (let ((*x* nil)
	(fn
	 (eval '(defgeneric dg-mc.fun.2 (x)
		  (:method-combination + :most-specific-first)
		  (:method + ((x integer)) (car (push 8 *x*)))
		  (:method + ((x rational)) (car (push 4 *x*)))
		  (:method + ((x number)) (car (push 2 *x*)))
		  (:method + ((x t)) (car (push 1 *x*)))))))
    (flet ((%f (y)
	       (let ((*x* nil))
		 (list (funcall fn y) *x*))))
    (values (%f 1) (%f 2/3) (%f 1.54) (%f 'a))))
  (15 (1 2 4 8)) (7 (1 2 4)) (3 (1 2)) (1 (1)))

(deftest defgeneric-method-combination.3
  (let ((*x* nil)
	(fn
	 (eval '(defgeneric dg-mc.fun.3 (x)
		  (:method-combination + :most-specific-last)
		  (:method + ((x integer)) (car (push 8 *x*)))
		  (:method + ((x rational)) (car (push 4 *x*)))
		  (:method + ((x number)) (car (push 2 *x*)))
		  (:method + ((x t)) (car (push 1 *x*)))))))
    (flet ((%f (y)
	       (let ((*x* nil))
		 (list (funcall fn y) *x*))))
    (values (%f 1) (%f 2/3) (%f 1.54) (%f 'a))))
  (15 (8 4 2 1)) (7 (4 2 1)) (3 (2 1)) (1 (1)))

(deftest defgeneric-method-combination.4
  (let ((fn
	 (eval '(defgeneric dg-mc.4 (x)
		  (:method-combination +)
		  (:method + ((x integer)) 1)
		  (:method :around ((x rational)) 'foo)
		  (:method + ((x number)) 1)
		  (:method + ((x symbol)) 2)
		  (:method + ((x t)) 4)))))
    (values
     (funcall fn 0)
     (funcall fn 4/3)
     (funcall fn 1.54)
     (funcall fn 'x)
     (funcall fn '(a b c))))
  foo foo 5 6 4)





    
		  
