;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Feb 15 11:54:05 2003
;;;; Contains: Tests for subtype relationships on integer types

(in-package :cl-test)

(deftest subtypep.fixnum-or-bignum
  (check-equivalence '(or fixnum bignum) 'integer)
  nil)

(deftest subtypep.fixnum.integer
  (check-equivalence `(integer ,most-negative-fixnum ,most-positive-fixnum)
		     'fixnum)
  nil)

(deftest subtypep.bignum.integer
  (check-equivalence
   `(or (integer * (,most-negative-fixnum))
	(integer (,most-positive-fixnum) *))
   'bignum)
  nil)

;;;;;;;

(deftest subtypep.integer.1
  (subtypep* '(integer 0 10) '(integer 0 20))
  t t)

(deftest subtypep.integer.2
  (subtypep* '(integer 0 10) '(integer 0 (10)))
  nil t)

(deftest subtypep.integer.3
  (subtypep* '(integer 10 100) 'integer)
  t t)

(deftest subtypep.integer.4
  (subtypep* 'integer '(integer 10 100))
  nil t)

(deftest subtypep.integer.5
  (subtypep* '(integer 10 *) 'integer)
  t t)

(deftest subtypep.integer.6
  (subtypep* 'integer '(integer 10 *))
  nil t)

(deftest subtypep.integer.7
  (subtypep* '(integer 10) 'integer)
  t t)

(deftest subtypep.integer.8
  (subtypep* 'integer '(integer 10))
  nil t)

(deftest subtypep.integer.9
  (subtypep* '(integer * 10) 'integer)
  t t)

(deftest subtypep.integer.10
  (subtypep* 'integer '(integer * 10))
  nil t)

(deftest subtypep.integer.11
  (subtypep* '(integer 10) '(integer 5))
  t t)

(deftest subtypep.integer.12
  (subtypep* '(integer 5) '(integer 10))
  nil t)

(deftest subtypep.integer.13
  (subtypep* '(integer 10 *) '(integer 5))
  t t)

(deftest subtypep.integer.14
  (subtypep* '(integer 5) '(integer 10 *))
  nil t)

(deftest subtypep.integer.15
  (subtypep* '(integer 10) '(integer 5 *))
  t t)

(deftest subtypep.integer.16
  (subtypep* '(integer 5 *) '(integer 10))
  nil t)

(deftest subtypep.integer.17
  (subtypep* '(integer 10 *) '(integer 5 *))
  t t)

(deftest subtypep.integer.18
  (subtypep* '(integer 5 *) '(integer 10 *))
  nil t)

(deftest subtypep.integer.19
  (subtypep* '(integer * 5) '(integer * 10))
  t t)

(deftest subtypep.integer.20
  (subtypep* '(integer * 10) '(integer * 5))
  nil t)

(deftest subtypep.integer.21
  (subtypep* '(integer 10 *) '(integer * 10))
  nil t)

(deftest subtypep.integer.22
  (subtypep* '(integer * 10) '(integer 10 *))
  nil t)

(deftest subtypep.integer.23
  (check-equivalence '(integer (9)) '(integer 10))
  nil)

(deftest subtypep.integer.24
  (check-equivalence '(integer * (11)) '(integer * 10))
  nil)

(deftest subtypep.integer.25
  (check-equivalence
   '(and (or (integer 0 10) (integer 20 30))
	 (or (integer 5 15) (integer 25 35)))
   '(or (integer 5 10) (integer 25 30)))
  nil)

(deftest subtypep.integer.26
  (check-equivalence
   '(and (integer 0 10) (integer 5 15))
   '(integer 5 10))
  nil)

(deftest subtypep.integer.27
  (check-equivalence
   '(or (integer 0 10) (integer 5 15))
   '(integer 0 15))
  nil)

(deftest subtypep.integer.28
  (check-equivalence
   '(and integer (not (eql 10)))
   '(or (integer * 9) (integer 11 *)))
  nil)

(deftest subtypep.integer.29
  (check-equivalence
   '(and integer (not (integer 1 10)))
   '(or (integer * 0) (integer 11 *)))
  nil)

(deftest subtypep.integer.30
  (check-equivalence
   '(and (integer -100 100) (not (integer 1 10)))
   '(or (integer -100 0) (integer 11 100)))
  nil)

(deftest subtypep.integer.31
  (check-equivalence
   '(and integer (real 4 10))
   '(integer 4 10))
  nil)

(deftest subtypep.integer.32
  (check-equivalence
   '(and (integer 4 *) (real * 10))
   '(integer 4 10))
  nil)

(deftest subtypep.integer.33
  (check-equivalence
   '(and (integer * 10) (real 4))
   '(integer 4 10))
  nil)
