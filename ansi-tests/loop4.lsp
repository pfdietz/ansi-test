;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Oct 27 22:46:39 2002
;;;; Contains: Tests for LOOP FOR-AS-EQUAL-THEN

(in-package :cl-test)

(deftest loop.4.1
  (loop
   for x = 1 then (1+ x)
   until (> x 5)
   collect x)
  (1 2 3 4 5))

(deftest loop.4.2
  (loop
   for i from 1 to 10
   for j = (1+ i) collect j)
  (2 3 4 5 6 7 8 9 10 11))


