;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 25 07:49:34 1998
;;;; Contains: Package test code, part 01

(in-package :cl-test)
(declaim (optimize (safety 3)))

;; Test find-symbol, with the various combinations of
;; package designators

(deftest find-symbol-1
  (ignore-errors (find-symbol "aBmAchb1c"))
  nil nil)

(deftest find-symbol-2
  (ignore-errors (find-symbol "aBmAchb1c" "CL"))
  nil nil)

(deftest find-symbol-3
  (ignore-errors (find-symbol "aBmAchb1c" "COMMON-LISP"))
  nil nil)

(deftest find-symbol-4
  (ignore-errors (find-symbol "aBmAchb1c" "KEYWORD"))
  nil nil)

(deftest find-symbol-5
  (ignore-errors (find-symbol "aBmAchb1c" "COMMON-LISP-USER"))
  nil nil)

(deftest find-symbol-6
  (ignore-errors (find-symbol "CAR" "CL"))
  car :external)

(deftest find-symbol-7
  (ignore-errors (find-symbol "CAR" "COMMON-LISP"))
  car :external)

(deftest find-symbol-8
  (ignore-errors (find-symbol "CAR" "COMMON-LISP-USER"))
  car :inherited)

(deftest find-symbol-9
  (ignore-errors (find-symbol "CAR" "CL-TEST"))
  car :inherited)

(deftest find-symbol-10
  (ignore-errors (find-symbol "TEST" "KEYWORD"))
  :test :external)

(deftest find-symbol-11
  (ignore-errors (find-symbol "FIND-SYMBOL-11" "CL-TEST"))
  find-symbol-11 :internal)

(deftest find-symbol-12
  (ignore-errors (find-symbol "FOO" #\A))
  A::FOO :external)

(deftest find-symbol-13
  (progn
    (intern "X" (find-package "A"))
    (ignore-errors (find-symbol "X" #\A)))
  A::X :internal)

(deftest find-symbol-14
  (ignore-errors (find-symbol "FOO" #\B))
  A::FOO :inherited)

(deftest find-symbol-15
  (ignore-errors (find-symbol "FOO" "B"))
  A::FOO :inherited)

(deftest find-symbol-16
  (ignore-errors (find-symbol "FOO" (find-package "B")))
  A::FOO :inherited)
