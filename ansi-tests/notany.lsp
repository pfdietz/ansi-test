;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct 18 07:14:14 2002
;;;; Contains: Tests for NOTANY

(in-package :cl-test)

(deftest notany.1
  (not-mv (notany #'identity nil))
  nil)

(deftest notany.2
  (not-mv (notany #'identity #()))
  nil)

(deftest notany.3
  (let ((count 0))
    (values
     (notany #'(lambda (x) (incf count) (if (>= x 10) x nil))
	    '(1 2 4 13 5 1))
     count))
  nil 4)

(deftest notany.4
  (not-mv (notany #'/= '(1 2 3 4) '(1 2 3 4 5)))
  nil)

(deftest notany.5
  (not-mv (notany #'/= '(1 2 3 4 5) '(1 2 3 4)))
  nil)

(deftest notany.6
  (notany #'/= '(1 2 3 4 5) '(1 2 3 4 6))
  nil)

(deftest notany.7
  (not-mv (notany #'(lambda (x y) (and x y))
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
  (not-mv (notany #'zerop #*11111111111111))
  nil)

(deftest notany.10
  (not-mv (notany #'zerop #*))
  nil)

(deftest notany.11
  (notany #'zerop #*1111111011111)
  nil)

(deftest notany.12
  (not-mv (notany #'(lambda (x) (not (eql x #\a))) "aaaaaaaa"))
  nil)

(deftest notany.13
  (not-mv (notany #'(lambda (x) (eql x #\a)) ""))
  nil)

(deftest notany.14
  (notany #'(lambda (x) (not (eql x #\a))) "aaaaaabaaaa")
  nil)

(deftest notany.15
  (not-mv (notany 'null '(1 2 3 4)))
  nil)

(deftest notany.16
  (notany 'null '(1 2 3 nil 5))
  nil)

(deftest notany.order.1
  (let ((i 0) a b)
    (values
     (not (notany (progn (setf a (incf i)) 'null)
		  (progn (setf b (incf i)) '(a b c))))
     i a b))
  nil 2 1 2)

;;; Error cases

(deftest notany.error.1
  (signals-error (notany 1 '(a b c)) type-error)
  t)

(deftest notany.error.2
  (signals-error (notany #\a '(a b c)) type-error)
  t)

(deftest notany.error.3
  (signals-error (notany #() '(a b c)) type-error)
  t)

(deftest notany.error.4
  (signals-error (notany #'null 'a) type-error)
  t)

(deftest notany.error.5
  (signals-error (notany #'null 100) type-error)
  t)

(deftest notany.error.6
  (signals-error (notany #'null 'a) type-error)
  t)

(deftest notany.error.7
  (signals-error (notany #'eq () 'a) type-error)
  t)

(deftest notany.error.8
  (signals-error (notany) program-error)
  t)

(deftest notany.error.9
  (signals-error (notany #'null) program-error)
  t)

(deftest notany.error.10
  (signals-error (locally (notany 1 '(a b c)) t) type-error)
  t)

(deftest notany.error.11
  (signals-error (notany #'cons '(a b c)) program-error)
  t)

(deftest notany.error.12
  (signals-error (notany #'cons '(a b c) '(1 2 4) '(g h j)) program-error)
  t)

(deftest notany.error.13
  (signals-error (notany #'car '(a b c)) type-error)
  t)