;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Oct 17 23:25:58 2002
;;;; Contains: Tests of EVERY

(in-package :cl-test)

(deftest every.1
  (notnot-mv (every #'identity nil))
  t)

(deftest every.2
  (notnot-mv (every #'identity #()))
  t)

(deftest every.3
  (let ((count 0))
    (values
     (every #'(lambda (x) (incf count) (< x 10))
	    '(1 2 4 13 5 1))
     count))
  nil 4)

(deftest every.4
  (notnot-mv (every #'= '(1 2 3 4) '(1 2 3 4 5)))
  t)

(deftest every.5
  (notnot-mv (every #'= '(1 2 3 4 5) '(1 2 3 4)))
  t)

(deftest every.6
  (every #'= '(1 2 3 4 5) '(1 2 3 4 6))
  nil)

(deftest every.7
  (notnot-mv (every #'(lambda (x y) (or x y))
		    '(nil t t nil t) #(t nil t t nil nil)))
  t)

(deftest every.8
  (let ((x '(1))
	(args nil))
    (loop for i from 1 below (1- (min 100 call-arguments-limit))
	  do (push x args)
	  always (apply #'every #'= args)))
  t)

(deftest every.9
  (notnot-mv (every #'zerop #*000000000000))
  t)

(deftest every.10
  (notnot-mv (every #'zerop #*))
  t)

(deftest every.11
  (every #'zerop #*0000010000)
  nil)

(deftest every.12
  (notnot-mv (every #'(lambda (x) (eql x #\a)) "aaaaaaaa"))
  t)

(deftest every.13
  (notnot-mv (every #'(lambda (x) (eql x #\a)) ""))
  t)

(deftest every.14
  (every #'(lambda (x) (eql x #\a)) "aaaaaabaaaa")
  nil)

(deftest every.15
  (every 'null '(nil nil t nil))
  nil)

(deftest every.16
  (notnot-mv (every 'null '(nil nil nil nil)))
  t)

(deftest every.order.1
  (let ((i 0) x y)
    (values
     (every (progn (setf x (incf i)) #'null)
	    (progn (setf y (incf i)) '(nil nil a nil)))
     i x y))
  nil 2 1 2)

(deftest every.order.2
  (let ((i 0) x y z)
    (values
     (every (progn (setf x (incf i)) #'equal)
	    (progn (setf y (incf i)) '(nil nil a nil))
	    (progn (setf z (incf i)) '(nil nil a b)))
     i x y z))
  nil 3 1 2 3)

;;; Error cases

(deftest every.error.1
  (classify-error (every 1 '(a b c)))
  type-error)

(deftest every.error.2
  (classify-error (every #\a '(a b c)))
  type-error)

(deftest every.error.3
  (classify-error (every #() '(a b c)))
  type-error)

(deftest every.error.4
  (classify-error (every #'null 'a))
  type-error)

(deftest every.error.5
  (classify-error (every #'null 100))
  type-error)

(deftest every.error.6
  (classify-error (every #'null 'a))
  type-error)

(deftest every.error.7
  (classify-error (every #'eq () 'a))
  type-error)
`
(deftest every.error.8
  (classify-error (every))
  program-error)

(deftest every.error.9
  (classify-error (every #'null))
  program-error)

(deftest every.error.10
  (classify-error (locally (every 1 '(a b c)) t))
  type-error)

(deftest every.error.11
  (classify-error (every #'cons '(a b c)))
  program-error)

(deftest every.error.12
  (classify-error (every #'cons '(a b c) '(1 2 3) '(4 5 6)))
  program-error)

(deftest every.error.13
  (classify-error (every #'car '(a b c)))
  type-error)
