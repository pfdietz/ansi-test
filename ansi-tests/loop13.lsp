;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Nov 17 12:37:45 2002
;;;; Contains: Tests of DO, DOING, RETURN in LOOP

(in-package :cl-test)

(deftest loop.13.1
  (loop do (return 10))
  10)

(deftest loop.13.2
  (loop doing (return 10))
  10)

(deftest loop.13.3
  (loop for i from 0 below 100 by 7
	when (> i 50) return i)
  56)



