;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Aug 17 20:54:48 2002
;;;; Contains: Tests for the MAP function

(in-package :cl-test)

(deftest map-array.1
  (map 'list #'1+ #(1 2 3 4))
  (2 3 4 5))

(deftest map-array.2
  (map 'vector #'+ #(1 2 3 4) #(6 6 6 6))
  #(7 8 9 10))

(deftest map-array.3
  (map 'vector #'+ #(1 2 3 4 5) #(6 6 6 6))
  #(7 8 9 10))

(deftest map-array.4
  (map 'vector #'+ #(1 2 3 4) #(6 6 6 6 6))
  #(7 8 9 10))

(deftest map-array.5
  (map '(vector *) #'+ #(1 2 3 4) #(6 6 6 6))
  #(7 8 9 10))

(deftest map-array.6
  (map '(vector * 4) #'+ #(1 2 3 4) #(6 6 6 6))
  #(7 8 9 10))

;;; (deftest map-array.7
;;;  (map 'array #'identity '(a b c d e f))
;;;  #(a b c d e f))

;;; (deftest map-array.8
;;;   (map 'simple-array #'identity '(a b c d e f))
;;;   #(a b c d e f))

(deftest map-array.9
  (map 'simple-vector #'identity '(a b c d e f))
  #(a b c d e f))

(deftest map-array.10
  (map 'simple-vector #'cons '(a b c d e f) #(1 2 3 4 5 6))
  #((a . 1) (b . 2) (c . 3) (d . 4) (e . 5) (f . 6)))

(deftest map-array.11
  (map 'vector #'identity '(#\a #\b #\c #\d #\e))
  #(#\a #\b #\c #\d #\e))

(deftest map-array.12
  (map 'vector #'identity "abcde")
  #(#\a #\b #\c #\d #\e))

(deftest map-array.13
  (map 'vector #'identity #*000001)
  #(0 0 0 0 0 1))

(deftest map-array.14
  (map 'list #'identity #*000001)
  (0 0 0 0 0 1))

(deftest map-bit-vector.15
  (map 'bit-vector #'identity '(0 0 0 0 0 1))
  #*000001)

(deftest map-bit-vector.16
  (map 'simple-bit-vector #'identity '(0 0 0 0 0 1))
  #*000001)

(deftest map-bit-vector.17
  (map '(vector bit) #'identity '(0 0 0 0 0 1))
  #*000001)

(deftest map-bit-vector.18
  (map '(simple-vector *) #'identity '(0 0 0 0 0 1))
  #*000001)

(deftest map-bit-vector.19
  (map '(bit-vector 6) #'identity '(0 0 0 0 0 1))
  #*000001)

(deftest map-bit-vector.20
  (map '(bit-vector *) #'identity '(0 0 0 0 0 1))
  #*000001)

(deftest map-bit-vector.21
  (map '(simple-bit-vector 6) #'identity '(0 0 0 0 0 1))
  #*000001)

(deftest map-bit-vector.22
  (map '(simple-bit-vector *) #'identity '(0 0 0 0 0 1))
  #*000001)

(deftest map-bit-vector.23
  (map '(vector bit 6) #'identity '(0 0 0 0 0 1))
  #*000001)

(deftest map-bit-vector.24
  (map '(vector bit *) #'identity '(0 0 0 0 0 1))
  #*000001)

(deftest map-bit-vector.25
  (map '(simple-vector 6) #'identity '(0 0 0 0 0 1))
  #*000001)

(deftest map-string.26
  (map 'string #'identity '(#\a #\b #\c #\d #\e))
  "abcde")

(deftest map-string.27
  (map 'string #'identity "abcde")
  "abcde")

(deftest map-string.28
  (map '(vector character) #'identity '(#\a #\b #\c #\d #\e))
  "abcde")

(deftest map-string.29
  (map '(vector character 5) #'identity '(#\a #\b #\c #\d #\e))
  "abcde")

(deftest map-string.30
  (map '(simple-vector 5) #'identity '(#\a #\b #\c #\d #\e))
  "abcde")

;;; Use a more elaborate form of the simple-array type specifier
;;; (deftest map-string.31
;;;  (map '(simple-array character *) #'identity "abcde")
;;;  "abcde")

;;; Use a more elaborate form of the simple-array type specifier
;;; (deftest map-string.32
;;;  (map '(simple-array character 5) #'identity "abcde")
;;;   "abcde")

(deftest map-nil.33
  (let ((a nil))
    (values (map nil #'(lambda (x) (push x a)) "abcdef") a))
  nil (#\f #\e #\d #\c #\b #\a))

(deftest map-nil.34
  (let ((a nil))
    (values (map nil #'(lambda (x) (push x a)) '(a b c d e)) a))
  nil (e d c b a))

(deftest map-nil.35
  (let ((a nil))
    (values (map nil #'(lambda (x) (push x a)) #(a b c d e)) a))
  nil (e d c b a))

(deftest map-nil.36
  (let ((a nil))
    (values (map nil #'(lambda (x) (push x a)) #*001011110) a))
  nil (0 1 1 1 1 0 1 0 0))

(deftest map-null.1
  (map 'null #'identity nil)
  nil)

(deftest map-cons.1
  (map 'cons #'identity '(a b c))
  (a b c))

(deftest map-error.1
  (handler-case (map 'symbol #'identity '(a b c))
		(error () :caught))
  :caught)

(deftest map-error.2
  (handler-case (map '(vector * 8) #'identity '(a b c))
		(type-error () :caught))
  :caught)

(deftest map-error.3
  (handler-case (map 'list #'identity '(a b . c))
		(type-error () :caught))
  :caught)






