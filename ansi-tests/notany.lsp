;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct 18 07:14:14 2002
;;;; Contains: Tests for NOTANY

(in-package :cl-test)

(deftest notany.1
  (not (notany #'identity nil))
  nil)

(deftest notany.2
  (not (notany #'identity #()))
  nil)

(deftest notany.3
  (let ((count 0))
    (values
     (notany #'(lambda (x) (incf count) (if (>= x 10) x nil))
	    '(1 2 4 13 5 1))
     count))
  nil 4)

(deftest notany.4
  (not (notany #'/= '(1 2 3 4) '(1 2 3 4 5)))
  nil)

(deftest notany.5
  (not (notany #'/= '(1 2 3 4 5) '(1 2 3 4)))
  nil)

(deftest notany.6
  (notany #'/= '(1 2 3 4 5) '(1 2 3 4 6))
  nil)

(deftest notany.7
  (not (notany #'(lambda (x y) (and x y))
	       '(nil t t nil t) #(t nil nil t nil nil)))
  nil)

(deftest notany.8
  (let* ((x '(1))
	 (args (list x)))
    (not
     (loop for i from 2 below (1- (min 100 call-arguments-limit))
	   do (push x args)
	   always (apply #'notany #'/= args))))
  nil)

(deftest notany.9
  (not (notany #'zerop #*11111111111111))
  nil)

(deftest notany.10
  (not (notany #'zerop #*))
  nil)

(deftest notany.11
  (notany #'zerop #*1111111011111)
  nil)

(deftest notany.12
  (not (notany #'(lambda (x) (not (eql x #\a))) "aaaaaaaa"))
  nil)

(deftest notany.13
  (not (notany #'(lambda (x) (eql x #\a)) ""))
  nil)

(deftest notany.14
  (notany #'(lambda (x) (not (eql x #\a))) "aaaaaabaaaa")
  nil)

(deftest notany.15
  (classify-error (notany 1 '(a b c)))
  type-error)

(deftest notany.16
  (classify-error (notany #\a '(a b c)))
  type-error)

(deftest notany.17
  (classify-error (notany #() '(a b c)))
  type-error)

(deftest notany.18
  (not (notany 'null '(1 2 3 4)))
  nil)

(deftest notany.19
  (notany 'null '(1 2 3 nil 5))
  nil)

(deftest notany.20
  (classify-error (notany #'null 'a))
  type-error)

(deftest notany.21
  (classify-error (notany #'null 100))
  type-error)

(deftest notany.22
  (classify-error (notany #'null 'a))
  type-error)

(deftest notany.23
  (classify-error (notany #'eq () 'a))
  type-error)
