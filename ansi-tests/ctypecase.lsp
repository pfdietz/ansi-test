;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct 18 23:05:10 2002
;;;; Contains: Tests of CTYPECASE

(in-package :cl-test)

(deftest ctypecase.1
  (let ((x 1))
    (ctypecase x (integer 'a) (t 'b)))
  a)

(deftest ctypecase.2
  (let ((x 1))
    (classify-error (ctypecase x (symbol 'a))))
  type-error)

(deftest ctypecase.3
  (let ((x 1))
    (ctypecase x (symbol 'a) (t 'b)))
  b)

(deftest ctypecase.4
  (let ((x 1))
    (ctypecase x (t (values)))))

(deftest ctypecase.5
  (let ((x 1))
    (ctypecase x (integer (values)) (t 'a))))

(deftest ctypecase.6
  (let ((x 1))
    (ctypecase x (bit 'a) (integer 'b)))
  a)

(deftest ctypecase.7
  (let ((x 1))
    (ctypecase x (t 'a)))
  a)

(deftest ctypecase.8
  (let ((x 1))
    (ctypecase x (t (values 'a 'b 'c))))
  a b c)

(deftest ctypecase.9
  (let ((x 1))
    (ctypecase x (integer (values 'a 'b 'c)) (t nil)))
  a b c)

(deftest ctypecase.10
  (let ((x 0) (y 1))
    (values
     (ctypecase y
       (bit     (incf x)   'a)
       (integer (incf x 2) 'b)
       (t       (incf x 4) 'c))
     x))
  a 1)

(deftest ctypecase.11
  (let ((x 1))
    (ctypecase x (integer) (t 'a)))
  nil)




    




