;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct 18 19:36:57 2002
;;;; Contains: Tests of WHEN

(in-package :cl-test)

(deftest when.1
  (when t)
  nil)

(deftest when.2
  (when nil 'a)
  nil)

(deftest when.3 (when t (values)))

(deftest when.4
  (when t (values 'a 'b 'c 'd))
  a b c d)

(deftest when.5
 (when nil (values))
 nil)

(deftest when.6
  (when nil (values 'a 'b 'c 'd))
  nil)

(deftest when.7
  (let ((x 0))
    (values
     (when t (incf x) 'a)
     x))
  a 1)

;;; (deftest when.error.1
;;;  (classify-error (when))
;;;  program-error)
