;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Dec 13 20:48:04 2002
;;;; Contains: Tests for COERCE

(in-package :cl-test)

(deftest coerce.1
  (loop for x in *universe*
	for type = (type-of x)
	unless (and (consp type) (eqt (car type) 'function))
	count (not (eq (coerce x type) x)))
  0)

(deftest coerce.2
  (loop for x in *universe*
	count (not (eq (coerce x t) x)))
  0)

(deftest coerce.3
  (loop for x in *universe*
	for class = (class-of x)
	count (and class (not (eq (coerce x class) x))))
  0)

(deftest coerce.4
  (loop for x in '(() #() #*)
	never (coerce x 'list))
  t)

(deftest coerce.5
  (loop for x in '((1 0) #(1 0) #*10)
	always (equal (coerce x 'list) '(1 0)))		   
  t)

(deftest coerce.6
  (loop for x in '(() #() #*)
	always (equalp (coerce x 'vector) #()))
  t)

(deftest coerce.7
  (loop for x in '((1 0) #(1 0) #*10)
	for y = (coerce x 'vector)
	always (and (equalp y #(1 0))
		    (vectorp y)))
  t)

(deftest coerce.8
  (loop for x in '((1 0) #(1 0) #*10)
	for y = (coerce x '(vector *))
	always (and (equalp y #(1 0))
		    (vectorp y)))
  t)

(deftest coerce.9
  (loop for x in '((1 0) #(1 0) #*10)
	for y = (coerce x '(vector * 2))
	always (and (equalp y #(1 0))
		    (vectorp y)))
  t)

(deftest coerce.10
  (values (coerce #\A 'character)
	  (coerce '|A| 'character)
	  (coerce "A" 'character))
  #\A #\A #\A)

(deftest coerce.11
  (loop with class = (find-class 'vector)
	for x in '((1 0) #(1 0) #*10)
	for y = (coerce x class)
	always (and (equalp y #(1 0))
		    (vectorp y)))
  t)

(deftest coerce.12
  (loop for x in '((1 0) #(1 0) #*10)
	for y = (coerce x 'bit-vector)
	always (and (equalp y #*10)
		    (bit-vector-p y)))
  t)

(deftest coerce.13
  (loop for x in '((#\a #\b #\c) "abc")
	for y = (coerce x 'string)
	always (and (stringp y)
		    (string= y "abc")))
  t)

(deftest coerce.14
  (loop for x in '((#\a #\b #\c) "abc")
	for y = (coerce x 'simple-string)
	always (and (typep y 'simple-string)
		    (string= y "abc")))
  t)

(deftest coerce.15
  (loop for x in '((1 0) #(1 0) #*10)
	for y = (coerce x 'simple-vector)
	always (and (equalp y #(1 0))
		    (simple-vector-p y)))
  t)

(deftest coerce.16
  (coerce 0 'integer)
  0)

(deftest coerce.17
  (coerce 0 'complex)
  0)

(deftest coerce.18
  (coerce 3 'complex)
  3)

(deftest coerce.19
  (coerce 5/3 'complex)
  5/3)

(deftest coerce.20
  (coerce 1.0 'complex)
  #c(1.0 0.0))

(deftest coerce.21
  (eqt (symbol-function 'car)
       (coerce 'car 'function))
  t)

(deftest coerce.22
  (funcall (coerce '(lambda () 10) 'function))
  10)

(deftest coerce.order.1
  (let ((i 0) a b)
    (values
     (coerce (progn (setf a (incf i)) 10)
	     (progn (setf b (incf i)) 'single-float))
     i a b))
  10.0f0 2 1 2)

;;; Error tests

;;; (deftest coerce.error.1
;;;  (classify-error (coerce -1 '(integer 0 100)))
;;;  type-error)

(deftest coerce.error.2
  (classify-error (coerce '(a b c) '(vector * 2)))
  type-error)

(deftest coerce.error.3
  (classify-error (coerce '(a b c) '(vector * 4)))
  type-error)

(deftest coerce.error.4
  (classify-error (coerce nil 'cons))
  type-error)

(deftest coerce.error.5
  (handler-case (eval '(coerce 'not-a-bound-function 'function))
		(error () :caught))
  :caught)

(deftest coerce.error.6
  (classify-error (coerce))
  program-error)

(deftest coerce.error.7
  (classify-error (coerce t))
  program-error)

(deftest coerce.error.8
  (classify-error (coerce 'x t 'foo))
  program-error)

(deftest coerce.error.9
  (classify-error (locally (coerce nil 'cons) t))
  type-error)
