;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct 18 23:35:07 2002
;;;; Contains: Tests of MULTIPLE-VALUE-CALL, MULTIPLE-VALUE-LIST

(in-package :cl-test)

(deftest multiple-value-call.1
  (multiple-value-call #'+ (values 1 2) (values) 3 (values 4 5 6))
  21)

(deftest multiple-value-call.2
  (multiple-value-call 'list)
  nil)

(deftest multiple-value-call.3
  (multiple-value-call 'list (floor 13 4))
  (3 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest multiple-value-list.1
  (multiple-value-list (values))
  nil)

(deftest multiple-value-list.2
  (multiple-value-list (values 'a))
  (a))

(deftest multiple-value-list.3
  (multiple-value-list (values 'a 'b))
  (a b))

(deftest multiple-value-list.4
  (not
   (loop
    for i from 0 below (min multiple-values-limit 100)
    for x = (make-list i :initial-element 'a)
    always (equal x (multiple-value-list (values-list x)))))
  nil)


