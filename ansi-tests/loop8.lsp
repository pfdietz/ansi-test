;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Nov 12 06:30:14 2002
;;;; Contains: Tests of LOOP local variable initialization

(in-package :cl-test)

(deftest loop.8.1
  (loop with x = 1 do (return x))
  1)

(deftest loop.8.2
  (loop with x = 1
        with y = (1+ x) do (return (list x y)))
  (1 2))

(deftest loop.8.3
  (let ((y 2))
    (loop with x = y
	  with y = (1+ x) do (return (list x y))))
  (2 3))

(deftest loop.8.4
  (let (a b)
    (loop with a = 1
	  and b = (list a)
	  and c = (list b)
	  return (list a b c)))
  (1 (nil) (nil)))


;;; type specs

(deftest loop.8.5
  (loop with a t = 1 return a)
  1)

(deftest loop.8.6
  (loop with a fixnum = 2 return a)
  2)

(deftest loop.8.7
  (loop with a float = 3.0 return a)
  3.0)

(deftest loop.8.8
  (loop with a of-type string = "abc" return a)
  "abc")

(deftest loop.8.9
  (loop with (a b) = '(1 2) return (list b a))
  (2 1))

(deftest loop.8.10
  (loop with (a b) of-type (fixnum fixnum) = '(3 4) return (+ a b))
  7)

(deftest loop.8.11
  (loop with a of-type fixnum return a)
  0)

(deftest loop.8.12
  (loop with a of-type float return a)
  0.0)

(deftest loop.8.13
  (loop with a of-type t return a)
  nil)

(deftest loop.8.14
  (loop with a t return a)
  nil)

(deftest loop.8.15
  (loop with a t and b t return (list a b))
  (nil nil))

(deftest loop.8.16
  (loop with (a b c) of-type (fixnum float t) return (list a b c))
  (0 0.0 nil))

(deftest loop.8.17
  (loop with nil = nil return nil)
  nil)

;;; The NIL block of a loop encloses the entire loop.

(deftest loop.8.18
  (loop with nil = (return t) return nil)
  t)

(deftest loop.8.19
  (loop with (nil a) = '(1 2) return a)
  2)

(deftest loop.8.20
  (loop with (a nil) = '(1 2) return a)
  1)

(deftest loop.8.21
  (loop with b = 3
	and (a nil) = '(1 2) return (list a b))
  (1 3))

(deftest loop.8.22
  (loop with b = 3
	and (nil a) = '(1 2) return (list a b))
  (2 3))

;;; The NIL block of a loop encloses the entire loop.

(deftest loop.8.23
  (loop
   with a = 1
   and  b = (return 2)
   return 3)
  2)

;;; Error cases

;;; According to the spec, it is a program-error for any variable to be bound
;;; more than once in loop clauses in the same loop form.

(deftest loop.8.error.1
  (classify-error
   (loop with a = 1
	 and  a = 2 return a))
  program-error)

(deftest loop.8.error.2
  (classify-error
   (loop with a = 1
	 with a = 2 return a))
  program-error)
