;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Feb 15 20:12:04 2003
;;;; Contains: Tests of CHECK-TYPE

(in-package :cl-test)

(deftest check-type.1
  (let ((x 'a))
    (values (check-type x symbol) x))
  nil a)

(deftest check-type.2
  (classify-error
   (let ((x 'a))
     (check-type x integer)))
  type-error)

(deftest check-type.3
  (let ((x 'a))
    (handler-bind
     ((type-error #'(lambda (c) (store-value 15 c))))
     (values (check-type x number) x)))
  nil 15)

(deftest check-type.4
  (let ((x 'a))
    (values (check-type x symbol "a symbol") x))
  nil a)

(deftest check-type.5
  (let ((x 'a))
    (handler-bind
     ((type-error #'(lambda (c) (store-value "abc" c))))
     (values (check-type x string "a string") x)))
  nil "abc")

(deftest check-type.6
  (let ((x 'a))
    (handler-bind
     ((type-error #'(lambda (c) (declare (ignore c)) (store-value 15 nil))))
     (values (check-type x number) x)))
  nil 15)

(deftest check-type.7
  (let ((x 'a))
    (handler-bind
     ((type-error #'(lambda (c) (declare (ignore c)) (store-value 15))))
     (values (check-type x number) x)))
  nil 15)

