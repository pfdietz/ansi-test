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
  t)

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

(deftest vector.type.31
  (let ((s (coerce "abc" 'simple-base-string)))
    (notnot-mv (typep s '(vector base-char))))
  t)

(deftest vector.type.32
  (let ((s (coerce "abc" 'simple-base-string)))
    (notnot-mv (typep s '(vector base-char 3))))
  t)

(deftest vector.type.33
  (let ((s (coerce "abc" 'simple-base-string)))
    (typep s '(vector base-char 2)))
  nil)

(deftest vector.type.34
  (let ((s (coerce "abc" 'simple-base-string)))
    (typep s '(vector base-char 4)))
  nil)

(deftest vector.type.35
  (let ((s (coerce "abc" 'simple-base-string)))
    (notnot-mv (typep s 'vector)))
  t)

(deftest vector.type.36
  (let ((s (coerce "abc" 'simple-base-string)))
    (notnot-mv (typep s '(vector *))))
  t)

(deftest vector.type.37
  (let ((s (coerce "abc" 'simple-base-string)))
    (notnot-mv (typep s '(vector * 3))))
  t)

(deftest vector.type.38
  (let ((s (coerce "abc" 'simple-base-string)))
    (notnot-mv (typep s '(vector * *))))
  t)

(deftest vector.type.39
  (let ((s (coerce "abc" 'simple-base-string)))
    (typep s '(vector t)))
  nil)

(deftest vector.type.40
  (let ((s (coerce "abc" 'simple-base-string)))
    (typep s '(vector t *)))
  nil)

;;;; Tests of the function VECTOR

(deftest vector.1
  (vector)
  #())

(deftest vector.2
  (vector 1 2 3)
  #(1 2 3))

(deftest vector.3
  (let* ((len (min 1000 (1- call-arguments-limit)))
	 (args (make-int-list len))
	 (v (apply #'vector args)))
    (and
     (typep v '(vector t))
     (typep v '(vector t *))
     (typep v `(vector t ,len))
     (typep v 'simple-vector)
     (typep v `(simple-vector ,len))
     (eql (length v) len)
     (loop for i from 0
	   for e across v
	   always (eql i e))
     t))
  t)

(deftest vector.4
  (notnot-mv (typep (vector) '(vector t 0)))
  t)
  
(deftest vector.5
  (notnot-mv (typep (vector) 'simple-vector))
  t)
  
(deftest vector.6
  (notnot-mv (typep (vector) '(simple-vector 0)))
  t)
  
(deftest vector.7
  (notnot-mv (typep (vector 1 2 3) 'simple-vector))
  t)
  
(deftest vector.8
  (notnot-mv (typep (vector 1 2 3) '(simple-vector 3)))
  t)

(deftest vector.9
  (typep (vector #\a #\b #\c) 'string)
  nil)
