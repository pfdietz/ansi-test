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
  (not-mv (some #'/= '(1 2 3 4 5) '(1 2 3 4 6)))
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
  (not-mv (some #'zerop #*1111111011111))
  nil)

(deftest some.12
  (some #'(lambda (x) (not (eql x #\a))) "aaaaaaaa")
  nil)

(deftest some.13
  (some #'(lambda (x) (eql x #\a)) "")
  nil)

(deftest some.14
  (not-mv (some #'(lambda (x) (not (eql x #\a))) "aaaaaabaaaa"))
  nil)

(deftest some.15
  (some 'null '(1 2 3 4))
  nil)

(deftest some.16
  (not-mv (some 'null '(1 2 3 nil 5)))
  nil)

(deftest some.order.1
  (let ((i 0) x y)
    (values
     (some (progn (setf x (incf i)) #'null)
	   (progn (setf y (incf i)) '(a b c d)))
     i x y))
  nil 2 1 2)

(deftest some.order.2
  (let ((i 0) x y z)
    (values
     (some (progn (setf x (incf i)) #'eq)
	   (progn (setf y (incf i)) '(a b c d))
	   (progn (setf z (incf i)) '(e f g h)))
     i x y z))
  nil 3 1 2 3)


(deftest some.error.1
  (classify-error (some 1 '(a b c)))
  type-error)

(deftest some.error.2
  (classify-error (some #\a '(a b c)))
  type-error)

(deftest some.error.3
  (classify-error (some #() '(a b c)))
  type-error)

(deftest some.error.4
  (classify-error (some #'null 'a))
  type-error)

(deftest some.error.5
  (classify-error (some #'null 100))
  type-error)

(deftest some.error.6
  (classify-error (some #'null 'a))
  type-error)

(deftest some.error.7
  (classify-error (some #'eq () 'a))
  type-error)

(deftest some.error.8
  (classify-error (some))
  program-error)

(deftest some.error.9
  (classify-error (some #'null))
  program-error)

(deftest some.error.10
  (classify-error (locally (some 1 '(a b c)) t))
  type-error)

(deftest some.error.11
  (classify-error (some #'cons '(a b c)))
  program-error)

(deftest some.error.12
  (classify-error (some #'car '(a b c)))
  type-error)

(deftest some.error.13
  (classify-error (some #'cons '(a b c) '(b c d) '(c d e)))
  program-error)


