;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Oct 26 13:45:45 2002
;;;; Contains: Tests of the FOR-AS-IN-LIST loop iteration control form,
;;;;      and of destructuring in loop forms

(in-package :cl-test)

(deftest loop.2.1
  (loop for x in '(1 2 3) sum x)
  6)

(deftest loop.2.2
  (loop for x in '(1 2 3 4)
	do (when (evenp x) (return x)))
  2)

(deftest loop.2.3
  (classify-error (loop for x in '(a . b) collect x))
  type-error)

(deftest loop.2.4
  (let ((x nil))
    (loop for e in '(a b c d) do (push e x))
    x)
  (d c b a))

(deftest loop.2.5
  (loop for e in '(a b c d e f) by #'cddr
	collect e)
  (a c e))

(deftest loop.2.6
  (loop for e in '(a b c d e f g) by #'cddr
	collect e)
  (a c e g))

(deftest loop.2.7
  (loop for e in '(a b c d e f)
	by #'(lambda (l) (and (cdr l) (cons (car l) (cddr l))))
	collect e)
  (a a a a a a))

(deftest loop.2.8
  (loop for (x . y) in '((a . b) (c . d) (e . f))
	collect (list x y))
  ((a b) (c d) (e f)))

(deftest loop.2.9
  (loop for (x nil y) in '((a b c) (d e f) (g h i))
	collect (list x y))
  ((a c) (d f) (g i)))

(deftest loop.2.10
  (loop for (x y) of-type fixnum in '((1 2) (3 4) (5 6))
	collect (+ x y))
  (3 7 11))

(deftest loop.2.10
  (loop for (x y) of-type fixnum in '((1 2) (3 4) (5 6))
	collect (+ x y))
  (3 7 11))

(deftest loop.2.11
  (loop for (x y) of-type (fixnum fixnum) in '((1 2) (3 4) (5 6))
	collect (+ x y))
  (3 7 11))


(deftest loop.2.11
  (loop for (x . y) of-type (fixnum . fixnum) in '((1 . 2) (3 . 4) (5 . 6))
	collect (+ x y))
  (3 7 11))

(deftest loop.2.12
  (classify-error
   (loop for x in '(a b c)
	 for x in '(d e f) collect x))
  programm-error)

(deftest loop.2.13
  (classify-error
   (loop for (x . x) in '((a b) (c d)) collect x))
  programm-error)

(deftest loop.2.14
  (loop for nil in nil do (return t))
  nil)





