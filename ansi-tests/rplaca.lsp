;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 21:29:43 2003
;;;; Contains: Tests of RPLACA

(in-package :cl-test)

(deftest rplaca.1
  (let ((x (cons 'a 'b)))
    (let ((y x))
      (and (eqt (rplaca x 'c) y)
	   (eqt x y)
	   (eqt (car x) 'c)
	   (eqt (cdr x) 'b))))
  t)

(deftest rplaca.order.1
  (let ((x (cons 'a 'b))
	(i 0) a b)
    (values
     (rplaca (progn (setf a (incf i)) x)
	     (progn (setf b (incf i)) 'c))
     i a b))
  (c . b) 2 1 2)

;; rplaca on a fixnum is a type error
(deftest rplaca.error.1
  (loop for x in *universe*
	thereis (and (not (consp x))
		     (not (eq (catch-type-error (rplaca x 1)) 'type-error))))
  nil)

(deftest rplaca.error.2
  (classify-error (rplaca))
  program-error)

(deftest rplaca.error.3
  (classify-error (rplaca (cons 'a 'b)))
  program-error)

(deftest rplaca.error.4
  (classify-error (rplaca (cons 'a 'b) (cons 'c 'd) 'garbage))
  program-error)

(deftest rplaca.error.5
  (classify-error (rplaca 'a 1))
  type-error)

(deftest rplaca.error.6
  (classify-error (locally (rplaca 'a 1) t))
  type-error)
