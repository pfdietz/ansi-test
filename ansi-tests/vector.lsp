;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Jan 23 06:32:02 2003
;;;; Contains: Tests of VECTOR (type and function)

(in-package :cl-test)

;;; More tests of type vector in make-array.lsp

(deftest vector.type.1
  (notnot-mv (typep #(a b c) 'vector))
  t)

(deftest vector.type.2
  (notnot-mv (typep #() 'vector))
  t)

(deftest vector.type.3
  (notnot-mv (typep "" 'vector))
  t)

(deftest vector.type.4
  (notnot-mv (typep "abcdef" 'vector))
  t)

(deftest vector.type.5
  (notnot-mv (typep #* 'vector))
  t)

(deftest vector.type.6
  (notnot-mv (typep #*011011101011 'vector))
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
  (notnot-mv (typep #(a b c) '(vector *)))
  t)

(deftest vector.type.11
  (notnot-mv (typep #(a b c) '(vector t)))
  t)

(deftest vector.type.12
  (notnot-mv (typep "abcde" '(vector *)))
  t)

(deftest vector.type.13
  (typep "abcdef" '(vector t))
  nil)

(deftest vector.type.14
  (notnot-mv (typep #*00110 '(vector *)))
  t)

(deftest vector.type.15
  (typep #*00110 '(vector t))
  nil)

(deftest vector.type.16
  (notnot-mv (typep #(a b c) '(vector * 3)))
  t)

(deftest vector.type.17
  (typep #(a b c) '(vector * 2))
  nil)

(deftest vector.type.18
  (typep #(a b c) '(vector * 4))
  nil)

(deftest vector.type.19
  (notnot-mv (typep #(a b c) '(vector t 3)))
  t)

(deftest vector.type.20
  (typep #(a b c) '(vector t 2))
  nil)

(deftest vector.type.21
  (typep #(a b c) '(vector t 4))
  nil)

(deftest vector.type.23
  (notnot-mv (typep #(a b c) '(vector t *)))
  nil)

(deftest vector.type.23a
  (notnot-mv (typep "abcde" '(vector * 5)))
  t)

(deftest vector.type.24
  (typep "abcde" '(vector * 4))
  nil)

(deftest vector.type.25
  (typep "abcde" '(vector * 6))
  nil)

(deftest vector.type.26
  (notnot-mv (typep "abcde" '(vector * *)))
  t)

(deftest vector.type.27
  (typep "abcde" '(vector t 5))
  nil)

(deftest vector.type.28
  (typep "abcde" '(vector t 4))
  nil)

(deftest vector.type.29
  (typep "abcde" '(vector t 6))
  nil)

(deftest vector.type.30
  (typep "abcde" '(vector t *))
  nil)
