;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Jan 23 06:32:02 2003
;;;; Contains: Tests of VECTOR (type and function)

(in-package :cl-test)

;;; More tests of type vector in make-array.lsp

(deftest vector.type.1
  (notnot (typep #(a b c) 'vector))
  t)

(deftest vector.type.2
  (notnot (typep #() 'vector))
  t)

(deftest vector.type.3
  (notnot (typep "" 'vector))
  t)

(deftest vector.type.4
  (notnot (typep "abcdef" 'vector))
  t)

(deftest vector.type.5
  (notnot (typep #* 'vector))
  t)

(deftest vector.type.6
  (notnot (typep #*011011101011 'vector))
  t)

(deftest vector.type.7
  (typep #0aNIL 'vector)
  nil)

(deftest vector.type.8
  (typep #2a((a b c d)) 'vector)
  nil)

(deftest vector.type.9
  (subtypep* 'vector 'array)
  t t)

(deftest vector.type.10
  (notnot (typep #(a b c) '(vector *)))
  t)

(deftest vector.type.11
  (notnot (typep #(a b c) '(vector t)))
  t)

(deftest vector.type.12
  (notnot (typep "abcde" '(vector *)))
  t)

(deftest vector.type.13
  (typep "abcdef" '(vector t))
  nil)

(deftest vector.type.14
  (notnot (typep #*00110 '(vector *)))
  t)

(deftest vector.type.15
  (typep #*00110 '(vector t))
  nil)



