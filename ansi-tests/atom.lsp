;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 21:28:09 2003
;;;; Contains: Tests of ATOM

(in-package :cl-test)

(deftest atom.order.1
  (let ((i 0))
    (values (atom (progn (incf i) '(a b))) i))
  nil 1)

(deftest atom.error.1
  (classify-error (atom))
  program-error)

(deftest atom.error.2
  (classify-error (atom 'a 'b))
  program-error)
