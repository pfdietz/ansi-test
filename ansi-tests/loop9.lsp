;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Nov 14 06:25:21 2002
;;;; Contains: Tests for loop value accumulation clauses

(in-package :cl-test)

;;; Tests of COLLECT, COLLECTING

(deftest loop.9.1
  (loop for x in '(2 3 4) collect (1+ x))
  (3 4 5))

(deftest loop.9.2
  (loop for x in '(2 3 4) collecting (1+ x))
  (3 4 5))

(deftest loop.9.3
  (loop for x in '(0 1 2)
	when (eql x 2) do (return 'good)
	collect x)
  good)

(deftest loop.9.4
  (loop for x in '(a b c)
	collect (list x) into foo
	finally (return (reverse foo)))
  ((c) (b) (a)))

(deftest loop.9.5
  (loop for x in '(a b c)
	collecting (list x) into foo
	finally (return (reverse foo)))
  ((c) (b) (a)))

(deftest loop.9.6
  (loop for x from 1 to 10
	when (evenp x) collect x into foo
	when (oddp x)  collect x into bar
	finally (return (list foo bar)))
  ((2 4 6 8 10) (1 3 5 7 9)))

(deftest loop.9.7
  (loop for x from 1 to 10
	collect (if (> x 5) (loop-finish) x))
  (1 2 3 4 5))

(deftest loop.9.8
  (loop for x from 1 to 20
	when (eql (mod x 5) 0) collect x into foo
	when (eql (mod x 5) 2) collect x into foo
	finally (return foo))
  (2 5 7 10 12 15 17 20))

(deftest loop.9.9
  (loop for x from 1 to 20
	when (eql (mod x 5) 0) collecting x into foo
	when (eql (mod x 5) 2) collecting x into foo
	finally (return foo))
  (2 5 7 10 12 15 17 20))

(deftest loop.9.10
  (classify-error
   (loop with foo = '(a b)
	 for x in '(c d) collect x into foo
	 finally (return foo)))
  program-error)

(deftest loop.9.11
  (classify-error
   (loop with foo = '(a b)
	 for x in '(c d) collecting x into foo
	 finally (return foo)))
  program-error)


;;; Tests of APPEND, APPENDING

(deftest loop.9.20
  (loop for x in '((a b) (c d) (e f g) () (i)) append x)
  (a b c d e f g i))

(deftest loop.9.21
  (loop for x in '((a b) (c d) (e f g) () (i)) appending x)
  (a b c d e f g i))

(deftest loop.9.22
  (loop for x in '((a) (b) (c . whatever)) append x)
  (a b c . whatever))

(deftest loop.9.23
  (loop for x in '((a) (b) (c . whatever)) appending x)
  (a b c . whatever))

;;; More to go here
