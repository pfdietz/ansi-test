;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Oct 17 23:25:58 2002
;;;; Contains: Tests of EVERY

(in-package :cl-test)

(deftest every.1
  (not (every #'identity nil))
  nil)

(deftest every.2
  (not (every #'identity #()))
  nil)

(deftest every.3
  (let ((count 0))
    (values
     (every #'(lambda (x) (incf count) (< x 10))
	    '(1 2 4 13 5 1))
     count))
  nil 4)

(deftest every.4
  (not (every #'= '(1 2 3 4) '(1 2 3 4 5)))
  nil)

(deftest every.5
  (not (every #'= '(1 2 3 4 5) '(1 2 3 4)))
  nil)

(deftest every.6
  (every #'= '(1 2 3 4 5) '(1 2 3 4 6))
  nil)

(deftest every.7
  (not (every #'(lambda (x y) (or x y))
	      '(nil t t nil t) #(t nil t t nil nil)))
  nil)

(deftest every.8
  (let ((x '(1))
	(args nil))
    (not
     (loop for i from 1 below (1- (min 100 call-arguments-limit))
	   do (push x args)
	   always (apply #'every #'= args))))
  nil)

(deftest every.9
  (not (every #'zerop #*000000000000))
  nil)

(deftest every.10
  (not (every #'zerop #*))
  nil)

(deftest every.11
  (every #'zerop #*0000010000)
  nil)

(deftest every.12
  (not (every #'(lambda (x) (eql x #\a)) "aaaaaaaa"))
  nil)

(deftest every.13
  (not (every #'(lambda (x) (eql x #\a)) ""))
  nil)

(deftest every.14
  (every #'(lambda (x) (eql x #\a)) "aaaaaabaaaa")
  nil)

(deftest every.15
  (classify-error (every 1 '(a b c)))
  type-error)

(deftest every.16
  (classify-error (every #\a '(a b c)))
  type-error)

(deftest every.17
  (classify-error (every #() '(a b c)))
  type-error)

(deftest every.18
  (every 'null '(nil nil t nil))
  nil)

(deftest every.19
  (not (every 'null '(nil nil nil nil)))
  nil)

(deftest every.20
  (classify-error (every #'null 'a))
  type-error)

(deftest every.21
  (classify-error (every #'null 100))
  type-error)

(deftest every.22
  (classify-error (every #'null 'a))
  type-error)

(deftest every.23
  (classify-error (every #'eq () 'a))
  type-error)
  
  









