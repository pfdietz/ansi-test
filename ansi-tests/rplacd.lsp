;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 21:30:28 2003
;;;; Contains: Tests of RPLACD

(in-package :cl-test)

(deftest rplacd.1
  (let ((x (cons 'a 'b)))
    (let ((y x))
      (and (eqt (rplacd x 'd) y)
	   (eqt x y)
	   (eqt (car x) 'a)
	   (eqt (cdr x) 'd))))
  t)

(deftest rplacd.order.1
  (let ((x (cons 'a 'b))
	(i 0) a b)
    (values
     (rplacd (progn (setf a (incf i)) x)
	     (progn (setf b (incf i)) 'c))
     i a b))
  (a . c) 2 1 2)

;; rplacd on a fixnum is a type error
(deftest rplacd.error.1
  (loop for x in *universe*
	thereis (and (not (consp x))
		     (not (eq (catch-type-error (rplacd x 1)) 'type-error))))
  nil)

(deftest rplacd.error.2
  (classify-error (rplacd))
  program-error)

(deftest rplacd.error.3
  (classify-error (rplacd (cons 'a 'b)))
  program-error)

(deftest rplacd.error.4
  (classify-error (rplacd (cons 'a 'b) (cons 'c 'd) 'garbage))
  program-error)

(deftest rplacd.error.5
  (classify-error (rplacd 'a 1))
  type-error)

(deftest rplacd.error.6
  (classify-error (locally (rplacd 'a 1) t))
  type-error)

