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
  (not-mv (notevery #'= '(1 2 3 4 5) '(1 2 3 4 6)))
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
  (not-mv (notevery #'zerop #*0000010000))
  nil)

(deftest notevery.12
  (notevery #'(lambda (x) (eql x #\a)) "aaaaaaaa")
  nil)

(deftest notevery.13
  (notevery #'(lambda (x) (eql x #\a)) "")
  nil)

(deftest notevery.14
  (not-mv (notevery #'(lambda (x) (eql x #\a)) "aaaaaabaaaa"))
  nil)

(deftest notevery.15
  (not-mv (notevery 'null '(nil nil t nil)))
  nil)

(deftest notevery.16
  (notevery 'null '(nil nil nil nil))
  nil)

(deftest notevery.order.1
  (let ((i 0) a b)
    (values
     (notevery (progn (setf a (incf i)) #'identity)
	       (progn (setf b (incf i)) '(a b c d)))
     i a b))
  nil 2 1 2)

;;; Error cases

(deftest notevery.error.1
  (classify-error (notevery 1 '(a b c)))
  type-error)

(deftest notevery.error.2
  (classify-error (notevery #\a '(a b c)))
  type-error)

(deftest notevery.error.3
  (classify-error (notevery #() '(a b c)))
  type-error)

(deftest notevery.error.4
  (classify-error (notevery #'null 'a))
  type-error)

(deftest notevery.error.5
  (classify-error (notevery #'null 100))
  type-error)

(deftest notevery.error.6
  (classify-error (notevery #'null 'a))
  type-error)

(deftest notevery.error.7
  (classify-error (notevery #'eq () 'a))
  type-error)

(deftest notevery.error.8
  (classify-error (notevery))
  program-error)

(deftest notevery.error.9
  (classify-error (notevery #'null))
  program-error)

(deftest notevery.error.10
  (classify-error (locally (notevery 1 '(a b c)) t))
  type-error)

(deftest notevery.error.11
  (classify-error (notevery #'cons '(a b c)))
  program-error)

(deftest notevery.error.12
  (classify-error (notevery #'cons '(a b c) '(1 2 4) '(g h j)))
  program-error)

(deftest notevery.error.13
  (classify-error (notevery #'car '(a b c)))
  type-error)