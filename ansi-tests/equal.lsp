;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Oct 17 21:38:16 2002
;;;; Contains: Tests for EQUAL

(in-package :cl-test)

(deftest equal.1
  (loop for x in *symbols*
	always (loop for y in *symbols*
		     always (if (eq x y) (equal x y)
			      (not (equal x y)))))
  t)

(deftest equal.2
  (equalt (cons 'a 'b) (cons 'a 'b))
  t)

(deftest equal.3
  (equalt (cons 'a 'c) (cons 'a 'b))
  nil)

(deftest equal.4
  (equalt (vector 1 2 3) (vector 1 2 3))
  nil)

(deftest equal.5
  (loop for c in *characters*
	always (loop for d in *characters*
		     always (if (eql c d) (equalt c d)
			      (not (equalt c d)))))
  t)

(deftest equal.6
  (equalt (make-pathname :name (copy-seq "foo"))
	  (make-pathname :name (copy-seq "foo")))
  t)

(deftest equal.7
  (equalt (make-pathname :name (copy-seq "foo"))
	  (make-pathname :name (copy-seq "bar")))
  nil)

(deftest equal.8
  (equalt (copy-seq "abcd") (copy-seq "abcd"))
  t)

(deftest equal.9
  (equalt (copy-seq "abcd") (copy-seq "abc"))
  nil)

(deftest equal.10
  (equalt (copy-seq "abcd") (copy-seq "ABCD"))
  nil)

(deftest equal.11
  (equalt (copy-seq #*000110) (copy-seq #*000110))
  t)

(deftest equal.12
  (equalt (copy-seq #*000110) (copy-seq #*000111))
  nil)

(deftest equal.order.1
  (let ((i 0) x y)
    (values
     (equal (setf x (incf i)) (setf y (incf i)))
     i x y))
  nil 2 1 2)

(deftest equal.error.1
  (classify-error (equal))
  program-error)

(deftest equal.error.2
  (classify-error (equal nil))
  program-error)

(deftest equal.error.3
  (classify-error (equal nil nil nil))
  program-error)
