;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Aug 28 21:00:44 2002
;;;; Contains: Tests for STABLE-SORT

(in-package :cl-test)

(deftest stable-sort-list.1
  (let ((a (list 1 4 2 5 3)))
    (stable-sort a #'<))
  (1 2 3 4 5))

(deftest stable-sort-list.2
  (let ((a (list 1 4 2 5 3)))
    (stable-sort a #'< :key #'-))
  (5 4 3 2 1))

(deftest stable-sort-list.3
  (let ((a (list 1 4 2 5 3)))
    (stable-sort a #'(lambda (x y) nil))
    (stable-sort a #'<))
  (1 2 3 4 5))

(deftest stable-sort-list.4
  (let ((a (copy-seq '((1 a) (2 a) (1 b) (2 b) (1 c) (2 c)))))
    (stable-sort a #'(lambda (x y) (< (car x) (car y)))))
  ((1 a) (1 b) (1 c) (2 a) (2 b) (2 c)))

(deftest stable-sort-list.5
  (let ((a (reverse (copy-seq '((1 a) (2 a) (1 b) (2 b) (1 c) (2 c))))))
    (stable-sort a #'(lambda (x y) (< (car x) (car y)))))
  ((1 c) (1 b) (1 a) (2 c) (2 b) (2 a)))

(deftest stable-sort-vector.1
  (let ((a (copy-seq #(1 4 2 5 3))))
    (stable-sort a #'<))
  #(1 2 3 4 5))

(deftest stable-sort-vector.2
  (let ((a (copy-seq #(1 4 2 5 3))))
    (stable-sort a #'< :key #'-))
  #(5 4 3 2 1))

(deftest stable-sort-vector.3
  (let ((a (copy-seq #(1 4 2 5 3))))
    (stable-sort a #'(lambda (x y) nil))
    (stable-sort a #'<))
  #(1 2 3 4 5))

(deftest stable-sort-bit-vector.1
  (let ((a (copy-seq #*10011101)))
    (stable-sort a #'<))
  #*00011111)

(deftest stable-sort-bit-vector.2
  (let ((a (copy-seq #*10011101)))
    (values (stable-sort a #'< :key #'-) a))
  #*11111000
  #*11111000)

(deftest stable-sort-string.1
  (let ((a (copy-seq "10011101")))
    (values (stable-sort a #'char<) a))
  "00011111"
  "00011111")

(deftest stable-sort-string.2
  (let ((a (copy-seq "10011101")))
    (values (stable-sort a #'char< :key #'(lambda (c) (if (eql c #\0) #\1 #\0))) a))
  "11111000"
  "11111000")







