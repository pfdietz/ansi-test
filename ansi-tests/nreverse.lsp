;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Aug 21 00:04:57 2002
;;;; Contains: Tests for NREVERSE

(in-package :cl-test)

(deftest nreverse-list.1
  (nreverse nil)
  nil)

(deftest nreverse-list.2
  (let ((x (copy-seq '(a b c))))
    (nreverse x))
  (c b a))

(deftest nreverse-vector.1
  (nreverse #())
  #())

(deftest nreverse-vector.2
  (let ((x (copy-seq #(a b c d e))))
    (nreverse x))
  #(e d c b a))

(deftest nreverse-nonsimple-vector.1
  (let ((x (make-array 0 :fill-pointer t :adjustable t)))
    (nreverse x))
  #())

(deftest nreverse-nonsimple-vector.2
  (let* ((x (make-array 5 :initial-contents '(1 2 3 4 5)
			:fill-pointer t :adjustable t))
	 (y (nreverse x)))
    (values y (equalt (type-of x) (type-of y))))
  #(5 4 3 2 1)
  t)

(deftest nreverse-nonsimple-vector.3
  (let* ((x (make-array 10 :initial-contents '(1 2 3 4 5 6 7 8 9 10)
			:fill-pointer 5))
	 (y (nreverse x)))
    (values y (equalt (type-of x) (type-of y))))
  #(5 4 3 2 1)
  t)

(deftest nreverse-bit-vector.1
  (nreverse #*)
  #*)

(deftest nreverse-bit-vector.2
  (let ((x (copy-seq #*000110110110)))
    (nreverse x))
  #*011011011000)

(deftest nreverse-bit-vector.3
  (let* ((x (make-array 10 :initial-contents '(0 0 0 1 1 0 1 0 1 0)
			:fill-pointer 5
			:element-type 'bit))
	 (y (nreverse x)))
    y)
  #*11000)

(deftest nreverse-string.1
  (nreverse "")
  "")

(deftest nreverse-string.2
  (let ((x (copy-seq "000110110110")))
    (nreverse x))
  "011011011000")

(deftest nreverse-string.3
  (let* ((x (make-array 10 :initial-contents "abcdefghij"
			:fill-pointer 5
			:element-type 'character))
	 (y (nreverse x)))
    y)
  "edcba")

(deftest nreverse-string.4
  (let* ((x (make-array 10 :initial-contents "abcdefghij"
			:fill-pointer 5
			:element-type 'base-char))
	 (y (nreverse x)))
    y)
  "edcba")

(deftest nreverse.order.1
  (let ((i 0))
    (values
     (nreverse (progn (incf i) (list 'a 'b 'c 'd)))
     i))
  (d c b a) 1)

(deftest nreverse.error.1
  (classify-error (nreverse 'a))
  type-error)

(deftest nreverse.error.2
  (classify-error (nreverse #\a))
  type-error)

(deftest nreverse.error.3
  (classify-error (nreverse 10))
  type-error)

(deftest nreverse.error.4
  (classify-error (nreverse 0.3))
  type-error)

(deftest nreverse.error.5
  (classify-error (nreverse 10/3))
  type-error)

(deftest nreverse.error.6
  (classify-error (nreverse))
  program-error)

(deftest nreverse.error.7
  (classify-error (nreverse nil nil))
  program-error)

(deftest nreverse.error.8
  (classify-error (locally (nreverse 'a) t))
  type-error)
