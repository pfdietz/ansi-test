;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Sep  7 07:47:43 2003
;;;; Contains: Tests of IMAGPART

(in-package :cl-test)

(deftest imagpart.error.1
  (classify-error (imagpart))
  program-error)

(deftest imagpart.error.2
  (classify-error (imagpart #c(1.0 2.0) nil))
  program-error)

(deftest imagpart.error.3
  (loop for x in *mini-universe*
	unless (or (numberp x)
		   (eq (eval `(classify-error (imagpart ',x))) 'type-error))
	collect x)
  nil)

(deftest imagpart.1
  (loop for x in *reals*
	for c = (complex 0 x)
	for ip = (imagpart c)
	unless (eql x ip)
	collect (list x c ip))
  nil)

(deftest imagpart.2
  (loop for x in *reals*
	for c = (complex 1 x)
	for ip = (imagpart c)
	unless (eql x ip)
	collect (list x c ip))
  nil)

(deftest imagpart.3
  (loop for x in *reals*
	for c = (complex x x)
	for ip = (imagpart c)
	unless (eql x ip)
	collect (list x c ip))
  nil)

(deftest imagpart.4
  (loop for x in *reals*
	for ip = (imagpart x)
	unless (eql (* 0 x) ip)
	collect (list x ip (* 0 x)))
  nil)


