;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct 25 19:07:19 2002
;;;; Contains: Tests of extended loop, part 1

(in-package :cl-test)

;;; Tests of variable initialization and stepping clauses

;;; for-as-arithmetic

(deftest loop.1.1
  (loop for x from 1 to 10 collect x)
  (1 2 3 4 5 6 7 8 9 10))

(deftest loop.1.2
  (loop for x from 6 downto 1 collect x)
  (6 5 4 3 2 1))

(deftest loop.1.3
  (loop for x from 1 to 1 collect x)
  (1))

(deftest loop.1.4
  (loop for x from 1 to 0 collect x)
  nil)

(deftest loop.1.5
  (loop for x to 5 collect x)
  (0 1 2 3 4 5))

(deftest loop.1.6
  (loop for x downfrom 5 to 0 collect x)
  (5 4 3 2 1 0))

(deftest loop.1.7
  (loop for x upfrom 1 to 5 collect x)
  (1 2 3 4 5))

(deftest loop.1.8
  (loop for x from 1.0 to 5.0 count x)
  5)

(deftest loop.1.9
  (loop for x from 1 to 9 by 2 collect x)
  (1 3 5 7 9))

(deftest loop.1.10
  (loop for x from 1 to 10 by 2 collect x)
  (1 3 5 7 9))

(deftest loop.1.11
  (loop for x to 10 from 1 collect x)
  (1 2 3 4 5 6 7 8 9 10))

(deftest loop.1.12
  (loop for x to 10 by 2 from 1 collect x)
  (1 3 5 7 9))

(deftest loop.1.13
  (loop for x by 2 to 10 from 1 collect x)
  (1 3 5 7 9))

(deftest loop.1.14
  (loop for x by 2 to 10 collect x)
  (0 2 4 6 8 10))

(deftest loop.1.15
  (loop for x to 10 by 2 collect x)
  (0 2 4 6 8 10))

(deftest loop.1.16
  (let ((n 0))
    (loop for x from (incf n) to (+ n 5) collect x))
  (1 2 3 4 5 6))

(deftest loop.1.17
  (let ((n 0))
    (loop for x to (+ n 5) from (incf n) collect x))
  (1 2 3 4 5))

(deftest loop.1.18
  (let ((n 0))
    (loop for x from (incf n) to (+ n 9) by (incf n) collect x))
  (1 3 5 7 9))

(deftest loop.1.19
  (let ((n 0))
    (loop for x from (incf n) by (incf n) to (+ n 9) collect x))
  (1 3 5 7 9 11))

(deftest loop.1.20
  (let ((a 0) (b 5) (c 1))
    (loop for x from a to b by c
	  collect (progn (incf a) (incf b 2) (incf c 3) x)))
  (0 1 2 3 4 5))



  