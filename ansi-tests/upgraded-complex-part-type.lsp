;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Nov 27 21:15:46 2004
;;;; Contains: Tests of UPGRADE-COMPLEX-PART-TYPE

(in-package :cl-test)

(compile-and-load "types-aux.lsp")

(defmacro def-ucpt-test (name types)
  `(deftest ,name
     (loop for type in (remove-duplicates ,types)
	   for upgraded-type = (upgraded-complex-part-type type)
	   for result = (append (check-all-subtypep type upgraded-type)
				(check-all-subtypep type 'real)
				(check-all-subtypep `(complex ,type) 'complex)
				(check-all-subtypep `(complex ,upgraded-type)
						    'complex)
				(check-all-subtypep `(complex ,type)
						    `(complex ,upgraded-type)))
	   when result
	   collect result)
     nil))

(def-ucpt-test upgraded-complex-part-type.1
  '(real integer rational ratio float short-float single-float
    double-float long-float fixnum bignum bit unsigned-byte signed-byte))

(def-ucpt-test upgraded-complex-part-type.2
  (mapcar #'find-class '(real float integer rational ratio)))

(def-ucpt-test upgraded-complex-part-type.3
  (mapcar #'class-of '(1.0s0 1.0f0 1.0d0 1.0l0)))

(def-ucpt-test upgraded-complex-part-type.4
  (loop for i from 1 to 100 collect `(unsigned-byte ,i)))

(def-ucpt-test upgraded-complex-part-type.5
  (loop for i from 1 to 100 collect `(signed-byte ,i)))

(def-ucpt-test upgraded-complex-part-type.6
  (loop for i = 1 then (* i 2)
	repeat 100
	collect (class-of i)))

;;; environment argument

(deftest upgraded-complex-part-type.7
  (loop for type in '(real integer rational float short-float
		      single-float double-float long-float fixnum
		      bignum bit unsigned-byte signed-byte)
	for ut1 = (upgraded-complex-part-type type)
	for ut2 = (upgraded-complex-part-type type nil)
	unless (equal ut1 ut2)
	collect (list type ut1 ut2))
  nil)

(deftest upgraded-complex-part-type.8
  (loop for type in '(real integer rational float short-float
		      single-float double-float long-float fixnum
		      bignum bit unsigned-byte signed-byte)
	for ut1 = (upgraded-complex-part-type type)
	for ut2 = (eval `(macrolet ((%m (&environment env)
					(list 'quote
					      (upgraded-complex-part-type ',type env))))
			   (%m)))
	unless (equal ut1 ut2)
	collect (list type ut1 ut2))
  nil)

;;; Error tests

(deftest upgraded-complex-part-type.error.1
  (signals-error (upgraded-complex-part-type) program-error)
  t)

(deftest upgraded-complex-part-type.error.2
  (signals-error (upgraded-complex-part-type 'real nil nil) program-error)
  t)
