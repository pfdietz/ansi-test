;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 22:48:36 2003
;;;; Contains: Tests of NTHCDR

(in-package :cl-test)

(deftest nthcdr.error.1
  (classify-error (nthcdr nil (copy-tree '(a b c d))))
  type-error)

(deftest nthcdr.error.2
  (classify-error (nthcdr 'a (copy-tree '(a b c d))))
  type-error)

(deftest nthcdr.error.3
  (classify-error (nthcdr 0.1 (copy-tree '(a b c d))))
  type-error)

(deftest nthcdr.error.4
  (classify-error (nthcdr #\A (copy-tree '(a b c d))))
  type-error)

(deftest nthcdr.error.5
  (classify-error (nthcdr '(a) (copy-tree '(a b c d))))
  type-error)

(deftest nthcdr.error.6
  (classify-error (nthcdr -10 (copy-tree '(a b c d))))
  type-error)

(deftest nthcdr.error.7
  (classify-error (nthcdr))
  program-error)

(deftest nthcdr.error.8
  (classify-error (nthcdr 0))
  program-error)

(deftest nthcdr.error.9
  (classify-error (nthcdr 0 nil nil))
  program-error)

(deftest nthcdr.error.10
  (classify-error (nthcdr 3 (cons 'a 'b)))
  type-error)

(deftest nthcdr.error.11
  (classify-error (locally (nthcdr 'a (copy-tree '(a b c d))) t))
  type-error)

(deftest nthcdr.1
  (nthcdr 0 (copy-tree '(a b c d . e)))
  (a b c d . e))

(deftest nthcdr.2
  (nthcdr 1 (copy-tree '(a b c d)))
  (b c d))

(deftest nthcdr.3
  (nthcdr 10 nil)
  nil)

(deftest nthcdr.4
  (nthcdr 4 (list 'a 'b 'c))
  nil)

(deftest nthcdr.5
  (nthcdr 1 (cons 'a 'b))
  b)

(deftest nthcdr.order.1
  (let ((i 0) x y)
    (values
     (nthcdr (setf x (incf i))
	     (progn (setf y (incf i)) '(a b c d)))
     i x y))
  (b c d) 2 1 2)

