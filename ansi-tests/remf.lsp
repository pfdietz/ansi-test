;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 07:38:18 2003
;;;; Contains: Tests of REMF

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest remf.1
  (let ((x nil))
    (values (remf x 'a) x))
  nil ())

(deftest remf.2
  (let ((x (list 'a 'b)))
    (values (not (null (remf x 'a))) x))
  t ())

(deftest remf.3
  (let ((x (list 'a 'b 'a 'c)))
    (values (not (null (remf x 'a))) x))
  t (a c))

(deftest remf.4
  (let ((x (list 'a 'b 'c 'd)))
    (values
     (and (remf x 'c) t)
     (loop
      for ptr on x by #'cddr count
      (not (eqt (car ptr) 'a)))))
  t 0)

(deftest remf.order.1
  (let ((i 0) x y
	(p (make-array 1 :initial-element (copy-list '(a b c d e f)))))
    (values
     (notnot
      (remf (aref p (progn (setf x (incf i)) 0))
	    (progn (setf y (incf i))
		   'c)))
     (aref p 0)
     i x y))
  t (a b e f) 2 1 2)

(deftest remf.order.2
  (let ((x  (copy-seq #(nil :a :b)))
	(pa (vector (list :a 1) (list :b 2) (list :c 3) (list :d 4)))
	(i 0))
    (values
     (not (remf (aref pa (incf i)) (aref x (incf i))))
     pa))
  nil #((:a 1) nil (:c 3) (:d 4)))

(def-macro-test remf.error.1 (remf x 'a))
