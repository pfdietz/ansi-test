;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct 18 07:20:12 2002
;;;; Contains: Tests for NOTEVERY

(in-package :cl-test)

(deftest notevery.1
  (notevery #'identity nil)
  nil)

(deftest notevery.2
  (notevery #'identity #())
  nil)

(deftest notevery.3
  (let ((count 0))
    (values
     (not (notevery #'(lambda (x) (incf count) (< x 10))
		    '(1 2 4 13 5 1)))
     count))
  nil 4)

(deftest notevery.4
  (notevery #'= '(1 2 3 4) '(1 2 3 4 5))
  nil)

(deftest notevery.5
  (notevery #'= '(1 2 3 4 5) '(1 2 3 4))
  nil)

(deftest notevery.6
  (not (notevery #'= '(1 2 3 4 5) '(1 2 3 4 6)))
  nil)

(deftest notevery.7
  (notevery #'(lambda (x y) (or x y))
	    '(nil t t nil t) #(t nil t t nil nil))
  nil)

(deftest notevery.8
  (let ((x '(1))
	(args nil))
    (not
     (loop for i from 1 below (1- (min 100 call-arguments-limit))
	   do (push x args)
	   always (not (apply #'notevery #'= args)))))
  nil)

(deftest notevery.9
  (notevery #'zerop #*000000000000)
  nil)

(deftest notevery.10
  (notevery #'zerop #*)
  nil)

(deftest notevery.11
  (not (notevery #'zerop #*0000010000))
  nil)

(deftest notevery.12
  (notevery #'(lambda (x) (eql x #\a)) "aaaaaaaa")
  nil)

(deftest notevery.13
  (notevery #'(lambda (x) (eql x #\a)) "")
  nil)

(deftest notevery.14
  (not (notevery #'(lambda (x) (eql x #\a)) "aaaaaabaaaa"))
  nil)

(deftest notevery.15
  (classify-error (notevery 1 '(a b c)))
  type-error)

(deftest notevery.16
  (classify-error (notevery #\a '(a b c)))
  type-error)

(deftest notevery.17
  (classify-error (notevery #() '(a b c)))
  type-error)

(deftest notevery.18
  (not (notevery 'null '(nil nil t nil)))
  nil)

(deftest notevery.19
  (notevery 'null '(nil nil nil nil))
  nil)

(deftest notevery.20
  (classify-error (notevery #'null 'a))
  type-error)

(deftest notevery.21
  (classify-error (notevery #'null 100))
  type-error)

(deftest notevery.22
  (classify-error (notevery #'null 'a))
  type-error)

(deftest notevery.23
  (classify-error (notevery #'eq () 'a))
  type-error)











  
  










