;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Sep  3 06:51:03 2003
;;;; Contains: Tests of GCD

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")
(compile-and-load "gcd-aux.lsp")

(deftest gcd.error.1
  (loop for x in *mini-universe*
	unless (or (integerp x)
		   (eq (eval `(classify-error (gcd ',x))) 'type-error))
	collect x)
  nil)

(deftest gcd.1
  (gcd)
  0)

(deftest gcd.2
  (loop for i = (random-fixnum)
	for a = (abs i)
	repeat 10000
	unless (and (eql a (gcd i))
		    (eql a (gcd 0 i)))
	collect i)
  nil)

(deftest gcd.3
  (loop for i = (random-from-interval 10000000000000000)
	for a = (abs i)
	repeat 10000
	unless (and (eql a (gcd i))
		    (eql a (gcd i 0)))
	collect i)
  nil)

(deftest gcd.4
  (loop for i = (random-fixnum)
	for j = (random-fixnum)
	repeat 1000
	unless (eql (my-gcd i j) (gcd i j))
	collect (list i j))
  nil)

(deftest gcd.5
  (let ((bound (ash 1 200)))
    (loop for i = (random-from-interval bound)
	  for j = (random-from-interval bound)
	  repeat 1000
	  unless (eql (my-gcd i j) (gcd i j))
	  collect (list i j)))
  nil)

(deftest gcd.6
  (loop for i = (random-fixnum)
	for j = (random-fixnum)
	for k = (random-fixnum)
	repeat 1000
	unless (eql (my-gcd i (my-gcd j k)) (gcd i j k))
	collect (list i j k))
  nil)

