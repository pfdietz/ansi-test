;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 24 21:31:55 2003
;;;; Contains: Tests of DEFGENERIC with :method-combination MIN

(in-package :cl-test)

(declaim (special *x*))

(deftest defgeneric-method-combination.min.1
  (let ((*x* nil)
	(fn
	 (eval '(defgeneric dg-mc.fun.min.1 (x)
		  (:method-combination min)
		  (:method min ((x integer)) (car (push 1 *x*)))
		  (:method min ((x rational)) (car (push 2 *x*)))
		  (:method min ((x number)) (car (push 3 *x*)))
		  (:method min ((x t)) (car (push 4 *x*)))))))
    (flet ((%f (y)
	       (let ((*x* nil))
		 (list (funcall fn y) *x*))))
    (values (%f 1) (%f 2/3) (%f 1.54) (%f 'a))))
  (1 (4 3 2 1)) (2 (4 3 2)) (3 (4 3)) (4 (4)))

(deftest defgeneric-method-combination.min.2
  (let ((*x* nil)
	(fn
	 (eval '(defgeneric dg-mc.fun.min.2 (x)
		  (:method-combination min :most-specific-first)
		  (:method min ((x integer)) (car (push 1 *x*)))
		  (:method min ((x rational)) (car (push 2 *x*)))
		  (:method min ((x number)) (car (push 3 *x*)))
		  (:method min ((x t)) (car (push 4 *x*)))))))
    (flet ((%f (y)
	       (let ((*x* nil))
		 (list (funcall fn y) *x*))))
    (values (%f 1) (%f 2/3) (%f 1.54) (%f 'a))))
  (1 (4 3 2 1)) (2 (4 3 2)) (3 (4 3)) (4 (4)))

(deftest defgeneric-method-combination.min.3
  (let ((*x* nil)
	(fn
	 (eval '(defgeneric dg-mc.fun.min.3 (x)
		  (:method-combination min :most-specific-last)
		  (:method min ((x integer)) (car (push 1 *x*)))
		  (:method min ((x rational)) (car (push 2 *x*)))
		  (:method min ((x number)) (car (push 3 *x*)))
		  (:method min ((x t)) (car (push 4 *x*)))))))
    (flet ((%f (y)
	       (let ((*x* nil))
		 (list (funcall fn y) *x*))))
    (values (%f 1) (%f 2/3) (%f 1.54) (%f 'a))))
  (1 (1 2 3 4)) (2 (2 3 4)) (3 (3 4)) (4 (4)))

(deftest defgeneric-method-combination.min.4
  (let ((fn
	 (eval '(defgeneric dg-mc.min.4 (x)
		  (:method-combination min)
		  (:method min ((x integer)) 1)
		  (:method :around ((x rational)) 'foo)
		  (:method min ((x number)) 2)
		  (:method min ((x symbol)) 3)
		  (:method min ((x t)) 4)))))
    (values
     (funcall fn 0)
     (funcall fn 4/3)
     (funcall fn 1.54)
     (funcall fn 'x)
     (funcall fn '(a b c))))
  foo foo 2 3 4)

(deftest defgeneric-method-combination.min.5
  (let ((fn
	 (eval '(defgeneric dg-mc.min.5 (x)
		  (:method-combination min)
		  (:method min ((x integer)) 1)
		  (:method :around ((x rational))
			   (list 'foo (call-next-method)))
		  (:method min ((x number)) 2)
		  (:method min ((x symbol)) 4)
		  (:method min ((x t)) 8)))))
    (values
     (funcall fn 0)
     (funcall fn 4/3)
     (funcall fn 1.54)
     (funcall fn 'x)
     (funcall fn '(a b c))))
  (foo 1) (foo 2) 2 4 8)

(deftest defgeneric-method-combination.min.6
  (let ((fn
	 (eval '(defgeneric dg-mc.min.6 (x)
		  (:method-combination min)
		  (:method min ((x integer)) 1)
		  (:method :around ((x rational))
			   (list 'foo (call-next-method)))
		  (:method :around ((x real))
			   (list 'bar (call-next-method)))
		  (:method min ((x number)) 2)
		  (:method min ((x symbol)) 4)
		  (:method min ((x t)) 8)))))
    (values
     (funcall fn 0)
     (funcall fn 4/3)
     (funcall fn 1.54)
     (funcall fn #c(1.0 2.0))
     (funcall fn 'x)
     (funcall fn '(a b c))))
  (foo (bar 1)) (foo (bar 2)) (bar 2) 2 4 8)

(deftest defgeneric-method-combination.min.7
  (let ((fn
	 (eval '(defgeneric dg-mc.min.7 (x)
		  (:method-combination min)
		  (:method min ((x dgmc-class-04)) 1)
		  (:method min ((x dgmc-class-03)) 2)
		  (:method min ((x dgmc-class-02)) 4)
		  (:method min ((x dgmc-class-01)) 8)))))
    (values
     (funcall fn (make-instance 'dgmc-class-01))
     (funcall fn (make-instance 'dgmc-class-02))
     (funcall fn (make-instance 'dgmc-class-03))
     (funcall fn (make-instance 'dgmc-class-04))))
  8 4 2 1)

(deftest defgeneric-method-combination.min.8
  (let ((fn
	 (eval '(defgeneric dg-mc.min.8 (x)
		  (:method-combination min)
		  (:method min ((x (eql 1000))) 0)
		  (:method :around ((x symbol)) (values))
		  (:method :around ((x integer)) (values 'a 'b 'c))
		  (:method :around ((x complex)) (call-next-method))
		  (:method :around ((x number)) (values 1 2 3 4 5 6))
		  (:method min ((x t)) 1)))))
    (values
     (multiple-value-list (funcall fn 'a))
     (multiple-value-list (funcall fn 10))
     (multiple-value-list (funcall fn #c(9 8)))
     (multiple-value-list (funcall fn '(a b c)))))
  () (a b c) (1 2 3 4 5 6) (1))
