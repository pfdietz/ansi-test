;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct 18 07:07:07 2002
;;;; Contains: Tests for SOME

(in-package :cl-test)

(deftest some.1
  (some #'identity nil)
  nil)

(deftest some.2
  (some #'identity #())
  nil)

(deftest some.3
  (let ((count 0))
    (values
     (some #'(lambda (x) (incf count) (if (>= x 10) x nil))
	    '(1 2 4 13 5 1))
     count))
  13 4)

(deftest some.4
  (some #'/= '(1 2 3 4) '(1 2 3 4 5))
  nil)

(deftest some.5
  (some #'/= '(1 2 3 4 5) '(1 2 3 4))
  nil)

(deftest some.6
  (not (some #'/= '(1 2 3 4 5) '(1 2 3 4 6)))
  nil)

(deftest some.7
  (some #'(lambda (x y) (and x y))
	'(nil t t nil t) #(t nil nil t nil nil))
  nil)

(deftest some.8
  (let ((x '(1))
	(args nil))
    (loop for i from 1 below (1- (min 100 call-arguments-limit))
	  do (push x args)
	  always (apply #'some #'/= args)))
  nil)

(deftest some.9
  (some #'zerop #*11111111111111)
  nil)

(deftest some.10
  (some #'zerop #*)
  nil)

(deftest some.11
  (not (some #'zerop #*1111111011111))
  nil)

(deftest some.12
  (some #'(lambda (x) (not (eql x #\a))) "aaaaaaaa")
  nil)

(deftest some.13
  (some #'(lambda (x) (eql x #\a)) "")
  nil)

(deftest some.14
  (not (some #'(lambda (x) (not (eql x #\a))) "aaaaaabaaaa"))
  nil)

(deftest some.15
  (classify-error (some 1 '(a b c)))
  type-error)

(deftest some.16
  (classify-error (some #\a '(a b c)))
  type-error)

(deftest some.17
  (classify-error (some #() '(a b c)))
  type-error)

(deftest some.18
  (some 'null '(1 2 3 4))
  nil)

(deftest some.19
  (not (some 'null '(1 2 3 nil 5)))
  nil)

(deftest some.20
  (classify-error (some #'null 'a))
  type-error)

(deftest some.21
  (classify-error (some #'null 100))
  type-error)

(deftest some.22
  (classify-error (some #'null 'a))
  type-error)

(deftest some.23
  (classify-error (some #'eq () 'a))
  type-error)











  
  










