;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct 18 23:02:23 2002
;;;; Contains: Tests of ETYPECASE

(in-package :cl-test)

(deftest etypecase.1
  (etypecase 1 (integer 'a) (t 'b))
  a)

(deftest etypecase.2
  (classify-error (etypecase 1 (symbol 'a)))
  type-error)

(deftest etypecase.3
  (etypecase 1 (symbol 'a) (t 'b))
  b)

(deftest etypecase.4
  (etypecase 1 (t (values))))

(deftest etypecase.5
  (etypecase 1 (integer (values)) (t 'a)))

(deftest etypecase.6
  (etypecase 1 (bit 'a) (integer 'b))
  a)

(deftest etypecase.7
  (etypecase 1 (t 'a))
  a)

(deftest etypecase.8
  (etypecase 1 (t (values 'a 'b 'c)))
  a b c)

(deftest etypecase.9
  (etypecase 1 (integer (values 'a 'b 'c)) (t nil))
  a b c)

(deftest etypecase.10
  (let ((x 0))
    (values
     (etypecase 1
       (bit     (incf x)   'a)
       (integer (incf x 2) 'b)
       (t       (incf x 4) 'c))
     x))
  a 1)

(deftest etypecase.11
  (etypecase 1 (integer) (t 'a))
  nil)

(deftest etypecase.12
  (etypecase 'a
    (number 'bad)
    (#.(find-class 'symbol nil) 'good))
  good)

