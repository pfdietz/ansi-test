;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Aug 21 00:11:24 2002
;;;; Contains: Tests for SORT

(in-package :cl-test)

(deftest sort-list.1
  (let ((a (list 1 4 2 5 3)))
    (sort a #'<))
  (1 2 3 4 5))

(deftest sort-list.2
  (let ((a (list 1 4 2 5 3)))
    (sort a #'< :key #'-))
  (5 4 3 2 1))

(deftest sort-list.3
  (let ((a (list 1 4 2 5 3)))
    (sort a #'(lambda (x y) nil))
    (sort a #'<))
  (1 2 3 4 5))

(deftest sort-vector.1
  (let ((a (copy-seq #(1 4 2 5 3))))
    (sort a #'<))
  #(1 2 3 4 5))

(deftest sort-vector.2
  (let ((a (copy-seq #(1 4 2 5 3))))
    (sort a #'< :key #'-))
  #(5 4 3 2 1))

(deftest sort-vector.3
  (let ((a (copy-seq #(1 4 2 5 3))))
    (sort a #'(lambda (x y) nil))
    (sort a #'<))
  #(1 2 3 4 5))

(deftest sort-bit-vector.1
  (let ((a (copy-seq #*10011101)))
    (sort a #'<))
  #*00011111)

(deftest sort-bit-vector.2
  (let ((a (copy-seq #*10011101)))
    (values (sort a #'< :key #'-) a))
  #*11111000
  #*11111000)

(deftest sort-string.1
  (let ((a (copy-seq "10011101")))
    (values (sort a #'char<) a))
  "00011111"
  "00011111")

(deftest sort-string.2
  (let ((a (copy-seq "10011101")))
    (values (sort a #'char< :key #'(lambda (c) (if (eql c #\0) #\1 #\0))) a))
  "11111000"
  "11111000")






