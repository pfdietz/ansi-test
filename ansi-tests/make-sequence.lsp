;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Sep 14 09:58:47 2002
;;;; Contains: Tests for MAKE-SEQUENCE

(in-package :cl-test)

(deftest make-sequence.1
  (let ((x (make-sequence 'list 4)))
    (and (eql (length x) 4)
	 (listp x)
	 (loop for e in x always (eql (car x) e))
	 t))
  t)

(deftest make-sequence.2
  (make-sequence 'list 4 :initial-element 'a)
  (a a a a))

(deftest make-sequence.3
  (let ((x (make-sequence 'cons 4)))
    (and (eql (length x) 4)
	 (listp x)
	 (loop for e in x always (eql (car x) e))
	 t))
  t)

(deftest make-sequence.4
  (make-sequence 'cons 4 :initial-element 'a)
  (a a a a))

(deftest make-sequence.5
  (make-sequence 'string 10 :initial-element #\a)
  "aaaaaaaaaa")

(deftest make-sequence.6
  (let ((s (make-sequence 'string 10)))
    (and (eql (length s) 10)
	 (loop for e across s always (eql e (aref s 0)))
	 t))
  t)

(deftest make-sequence.7
  (make-sequence 'simple-string 10 :initial-element #\a)
  "aaaaaaaaaa")

(deftest make-sequence.8
  (let ((s (make-sequence 'simple-string 10)))
    (and (eql (length s) 10)
	 (loop for e across s always (eql e (aref s 0)))
	 t))
  t)

(deftest make-sequence.9
  (make-sequence 'null 0)
  nil)

(deftest make-sequence.10
  (let ((x (make-sequence 'vector 10)))
    (and (eql (length x) 10)
	 (loop for e across x always (eql e (aref x 0)))
	 t))
  t)

(deftest make-sequence.11
  (let* ((u (list 'a))
	 (x (make-sequence 'vector 10 :initial-element u)))
    (and (eql (length x) 10)
	 (loop for e across x always (eql e u))
	 t))
  t)

(deftest make-sequence.12
  (let ((x (make-sequence 'simple-vector 10)))
    (and (eql (length x) 10)
	 (loop for e across x always (eql e (aref x 0)))
	 t))
  t)

(deftest make-sequence.13
  (let* ((u (list 'a))
	 (x (make-sequence 'simple-vector 10 :initial-element u)))
    (and (eql (length x) 10)
	 (loop for e across x always (eql e u))
	 t))
  t)

(deftest make-sequence.14
  (let ((x (make-sequence '(vector *) 10)))
    (and (eql (length x) 10)
	 (loop for e across x always (eql e (aref x 0)))
	 t))
  t)

(deftest make-sequence.15
  (let* ((u (list 'a))
	 (x (make-sequence '(vector *) 10 :initial-element u)))
    (and (eql (length x) 10)
	 (loop for e across x always (eql e u))
	 t))
  t)

(deftest make-sequence.16
  (let ((x (make-sequence '(simple-vector *)  10)))
    (and (eql (length x) 10)
	 (loop for e across x always (eql e (aref x 0)))
	 t))
  t)

(deftest make-sequence.17
  (let* ((u (list 'a))
	 (x (make-sequence '(simple-vector *) 10 :initial-element u)))
    (and (eql (length x) 10)
	 (loop for e across x always (eql e u))
	 t))
  t)

(deftest make-sequence.18
  (let ((x (make-sequence '(string *) 10)))
    (and (eql (length x) 10)
	 (loop for e across x always (eql e (aref x 0)))
	 t))
  t)

(deftest make-sequence.19
  (let* ((u #\a)
	 (x (make-sequence '(string *) 10 :initial-element u)))
    (and (eql (length x) 10)
	 (loop for e across x always (eql e u))
	 t))
  t)

(deftest make-sequence.20
  (let ((x (make-sequence '(simple-string *)  10)))
    (and (eql (length x) 10)
	 (loop for e across x always (eql e (aref x 0)))
	 t))
  t)

(deftest make-sequence.21
  (let* ((u #\a)
	 (x (make-sequence '(simple-string *) 10 :initial-element u)))
    (and (eql (length x) 10)
	 (loop for e across x always (eql e u))
	 t))
  t)

(deftest make-sequence.22
  (make-sequence '(vector * 5) 5 :initial-element 'a)
  #(a a a a a))

(deftest make-sequence.23
  (make-sequence '(vector fixnum 5) 5 :initial-element 1)
  #(1 1 1 1 1))

(deftest make-sequence.24
  (make-sequence '(vector (integer 0 255) 5) 5 :initial-element 17)
  #(17 17 17 17 17))

(deftest make-sequence.25
  (make-sequence '(simple-vector 5) 5 :initial-element 'a)
  #(a a a a a))

(deftest make-sequence.26
  (equalp (make-sequence 'string 5) (make-string 5))
  t)

;;; Keyword tests

(deftest make-sequence.allow-other-keys.1
  (make-sequence 'list 5 :allow-other-keys t :initial-element 'a :bad t)
  (a a a a a))

(deftest make-sequence.allow-other-keys.2
  (make-sequence 'list 5 :initial-element 'a :bad t :allow-other-keys t)
  (a a a a a))

(deftest make-sequence.allow-other-keys.3
  (make-sequence 'list 5 :initial-element 'a :allow-other-keys t)
  (a a a a a))

(deftest make-sequence.allow-other-keys.4
  (make-sequence 'list 5 :initial-element 'a :allow-other-keys nil)
  (a a a a a))

(deftest make-sequence.allow-other-keys.5
  (make-sequence 'list 5 :initial-element 'a :allow-other-keys t
		 :allow-other-keys nil :bad t)
  (a a a a a))

(deftest make-sequence.keywords.6
  (make-sequence 'list 5 :initial-element 'a :initial-element 'b)
  (a a a a a))

;;; Tests for errors

(deftest make-sequence.error.1
  (classify-error (make-sequence 'symbol 10))
  type-error)

(deftest make-sequence.error.2
  (classify-error (make-sequence 'null 1))
  type-error)

(deftest make-sequence.error.3
  (classify-error (make-sequence '(vector * 4) 3))
  type-error)

(deftest make-sequence.error.4
  (classify-error (make-sequence '(vector * 2) 3))
  type-error)

(deftest make-sequence.error.5
  (classify-error (make-sequence '(string 4) 3))
  type-error)

(deftest make-sequence.error.6
  (classify-error (make-sequence '(simple-string 2) 3))
  type-error)

(deftest make-sequence.error.7
  (classify-error (make-sequence 'cons 0))
  type-error)

(deftest make-sequence.error.8
  (classify-error (make-sequence))
  program-error)

(deftest make-sequence.error.9
  (classify-error (make-sequence 'list))
  program-error)

(deftest make-sequence.error.10
  (classify-error (make-sequence 'list 10 :bad t))
  program-error)

(deftest make-sequence.error.11
  (classify-error (make-sequence 'list 10 :bad t :allow-other-keys nil))
  program-error)

(deftest make-sequence.error.12
  (classify-error (make-sequence 'list 10 :initial-element))
  program-error)

(deftest make-sequence.error.13
  (classify-error (make-sequence 'list 10 0 0))
  program-error)

(deftest make-sequence.error.14
  (classify-error (locally (make-sequence 'symbol 10) t))
  type-error)

;;; Order of execution tests

(deftest make-sequence.order.1
  (let ((i 0) a b c)
    (values
     (make-sequence (progn (setf a (incf i)) 'list)
		    (progn (setf b (incf i)) 5)
		    :initial-element (progn (setf c (incf i)) 'a))
     i a b c))
  (a a a a a) 3 1 2 3)

(deftest make-sequence.order.2
  (let ((i 0) a b c d e)
    (values
     (make-sequence (progn (setf a (incf i)) 'list)
		    (progn (setf b (incf i)) 5)
		    :allow-other-keys (setf c (incf i))
		    :initial-element (progn (setf d (incf i)) 'a)
		    :foo (setf e (incf i)))
     i a b c d e))
  (a a a a a) 5 1 2 3 4 5)
