;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jun 14 07:23:03 2003
;;;; Contains: Tests of TREE-EQUAL

(in-package :cl-test)

(deftest tree-equal.1
  (notnot-mv (tree-equal 'a 'a))
  t)

(deftest tree-equal.2
  (tree-equal 'a 'b)
  nil)

(deftest tree-equal.3
  (notnot-mv (tree-equal (list 'a 'b (list 'c 'd))
			 (list 'a 'b (list 'c 'd))))
  t)

(deftest tree-equal.4
  (tree-equal '(a b c d) '(a b c e))
  nil)

(deftest tree-equal.5
  (notnot-mv (tree-equal 1 2 :test #'<))
  t)
  
(deftest tree-equal.6
  (notnot-mv (tree-equal 1 2 :test #'(lambda (x y) (values (< x y) t))))
  t)

(deftest tree-equal.7
  (tree-equal 1 2 :test #'>)
  nil)
  
(deftest tree-equal.8
  (tree-equal (list 1) 2 :test (constantly t))
  nil)

(deftest tree-equal.9
  (tree-equal (list 1) (list 2)
	      :test #'(lambda (x y) (or (and (consp x) (consp y))
					(eql x y))))
  nil)

(deftest tree-equal.10
  (notnot-mv (tree-equal '(10 20 . 30) '(11 22 . 34) :test #'<))
  t)

(deftest tree-equal.11
  (let* ((x (list 'a 'b))
	 (y (list x x))
	 (z (list (list 'a 'b) (list 'a 'b))))
    (notnot-mv (tree-equal y z)))
  t)

(deftest tree-equal.12
  (tree-equal 'a '(a b))
  nil)

(deftest tree-equal.13
  (tree-equal '(a) '(a b))
  nil)

(deftest tree-equal.14
  (tree-equal '(a b) '(a))
  nil)

(deftest tree-equal.15
  (let ((x (vector 'a 'b 'c))
	(y (vector 'a' 'b 'c)))
    (tree-equal x y))
  nil)

(deftest tree-equal.16
  (let ((x (copy-seq ""))
	(y (copy-seq "")))
    (tree-equal x y))
  nil)

;;; Keywords tests

(deftest tree-equal.allow-other-keys.1
  (notnot-mv (tree-equal '(a b) (list 'a 'b) :allow-other-keys nil))
  t)

(deftest tree-equal.allow-other-keys.2
  (tree-equal '(a b) (list 'a 'c) :allow-other-keys nil :test #'eql)
  nil)

(deftest tree-equal.allow-other-keys.3
  (tree-equal '(a b) (list 'a 'z) :allow-other-keys t :foo t)
  nil)

(deftest tree-equal.allow-other-keys.4
  (notnot-mv (tree-equal '(a b) (list 'a 'b) :allow-other-keys t
			 :allow-other-keys nil :foo t))
  t)

;;; Error tests

(deftest tree-equal.error.1
  (classify-error (tree-equal))
  program-error)

(deftest tree-equal.error.2
  (classify-error (tree-equal '(a b)))
  program-error)

(deftest tree-equal.error.3
  (classify-error (tree-equal '(a b) '(a b) (gensym) t))
  program-error)

(deftest tree-equal.error.4
  (classify-error (tree-equal '(a b) '(a b) (gensym) t :allow-other-keys nil))
  program-error)







    