;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Aug 18 10:10:04 2002
;;;; Contains: Tests for the MAP-INTO function

(in-package :cl-test)

(deftest map-into-list.1
  (let ((a (copy-seq '(a b c d e f)))
	(b nil))
    (map-into a #'(lambda (x) (push x b) x)  '(1 2 3 4 5 6))
    (values a b))
  (1 2 3 4 5 6)
  (6 5 4 3 2 1))

(deftest map-into-list.2
  (let ((a (copy-seq '(a b c d e f g))))
    (map-into a #'identity '(1 2 3))
    a)
  (1 2 3 d e f g))

(deftest map-into-list.3
  (let ((a (copy-seq '(a b c))))
    (map-into a #'identity '(1 2 3 4 5 6))
    a)
  (1 2 3))

(deftest map-into-list.4
  (let ((a (copy-seq '(a b c d e f)))
	(b nil))
    (map-into a #'(lambda (x y) (let ((z (+ x y))) (push z b) z))
	      '(1 2 3 4 5 6)
	      '(10 11 12 13 14 15))
    (values a b))
  (11 13 15 17 19 21)
  (21 19 17 15 13 11))

(deftest map-into-list.5
  (let ((a (copy-seq '(a b c d e f))))
    (map-into a 'identity '(1 2 3 4 5 6))
    a)
  (1 2 3 4 5 6))

(deftest map-into-list.6
  (let ((b nil))
    (values
     (map-into nil #'(lambda (x y) (let ((z (+ x y))) (push z b) z))
	       '(1 2 3 4 5 6)
	       '(10 11 12 13 14 15))
     b))
  nil nil)

(deftest map-into-list.7
  (let ((a (copy-seq '(a b c d e f))))
    (map-into a #'(lambda () 1))
    a)
  (1 1 1 1 1 1))

(deftest map-into-array.1
  (let ((a (copy-seq #(a b c d e f)))
	b)
    (map-into a #'(lambda (x) (push x b) x) '(1 2 3 4 5 6))
    (values a b))
  #(1 2 3 4 5 6)
  (6 5 4 3 2 1))

(deftest map-into-array.2
  (let ((a (copy-seq #(a b c d e f g h)))
	b)
    (map-into a #'(lambda (x) (push x b) x) '(1 2 3 4 5 6))
    (values a b))
  #(1 2 3 4 5 6 g h)
  (6 5 4 3 2 1))

(deftest map-into-array.3
  (let ((a (copy-seq #(a b c d)))
	b)
    (map-into a #'(lambda (x) (push x b) x) '(1 2 3 4 5 6))
    (values a b))
  #(1 2 3 4)
  (4 3 2 1))

(deftest map-into-array.4
  (let ((a (copy-seq #(a b c d e f)))
	b)
    (map-into a #'(lambda (x) (push x b) x) #(1 2 3 4 5 6))
    (values a b))
  #(1 2 3 4 5 6)
  (6 5 4 3 2 1))

(deftest map-into-array.5
  (let ((a (copy-seq #(a b c d e f g h)))
	b)
    (map-into a #'(lambda (x) (push x b) x) #(1 2 3 4 5 6))
    (values a b))
  #(1 2 3 4 5 6 g h)
  (6 5 4 3 2 1))

(deftest map-into-array.6
  (let ((a (copy-seq #(a b c d)))
	b)
    (map-into a #'(lambda (x) (push x b) x) #(1 2 3 4 5 6))
    (values a b))
  #(1 2 3 4)
  (4 3 2 1))

;;; Tests of mapping into arrays with fill pointers
(deftest map-into-array.7
  (let ((a (make-array 6 :initial-element 'x
		       :fill-pointer 3)))
    (map-into a #'identity '(1 2 3))
    a)
  #(1 2 3))

(deftest map-into-array.8
  (let ((a (make-array 6 :initial-element 'x
		       :fill-pointer 3)))
    (map-into a #'identity '(1 2))
    a)
  #(1 2))

(deftest map-into-array.9
  (let ((a (make-array 6 :initial-element 'x
		       :fill-pointer 3)))
    (map-into a #'identity '(1 2 3 4 5))
    a)
  #(1 2 3 4 5))

(deftest map-into-array.10
  (let ((a (make-array 6 :initial-element 'x
		       :fill-pointer 3)))
    (map-into a #'(lambda () 'y))
    a)
  #(y y y y y y))

;;; mapping into strings

(deftest map-into-string.1
  (let ((a (copy-seq "abcdef")))
    (map-into a #'identity "123456")
    (values (not (not (stringp a))) a))
  t
  "123456")

(deftest map-into-string.2
  (let ((a (copy-seq "abcdef")))
    (map-into a #'identity "1234")
    (values (not (not (stringp a))) a))
  t
  "1234ef")

(deftest map-into-string.3
  (let ((a (copy-seq "abcd")))
    (map-into a #'identity "123456")
    (values (not (not (stringp a))) a))
  t
  "1234")

(deftest map-into-error.1
  (handler-case (map-into 'a #'(lambda () nil))
		(type-error (c) 'type-error)
		(error (c) c))
  type-error)

(deftest map-into-error.2
  (handler-case (map-into nil #'identity 'a)
		(type-error (c) 'type-error)
		(error (c) c))
  type-error)

(deftest map-into-error.3
  (handler-case (map-into (copy-seq '(a b c)) #'cons '(d e f) 100)
		(type-error (c) 'type-error)
		(error (c) c))
  type-error)





