;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Nov 10 21:13:04 2002
;;;; Contains: Tests for LOOP-AS-HASH forms

(in-package :cl-test)

(defparameter *loop.6.alist*
  '((a . 1) (b . 2) (c . 3)))

(defparameter *loop.6.alist.2*
  '(("a" . 1) ("b" . 2) ("c" . 3)))

(defparameter *loop.6.alist.3*
  '(((a1 . a2) . 1) ((b1 . b2) . 2) ((c1 . c2) . 3)))

(defparameter *loop.6.hash.1*
  (let ((table (make-hash-table :test #'eq)))
    (loop for (key . val) in *loop.6.alist*
	  do (setf (gethash key table) val))
    table))

(defparameter *loop.6.hash.2*
  (let ((table (make-hash-table :test #'eql)))
    (loop for (key . val) in *loop.6.alist*
	  do (setf (gethash key table) val))
    table))

(defparameter *loop.6.hash.3*
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key . val) in *loop.6.alist.3*
	  do (setf (gethash key table) val))
    table))

;;; (defparameter *loop.6.hash.4*
;;;  (let ((table (make-hash-table :test #'equalp)))
;;;    (loop for (key . val) in *loop.6.alist.2*
;;;	  do (setf (gethash key table) val))
;;;    table))

(defparameter *loop.6.hash.5*
  (let ((table (make-hash-table :test #'eql)))
    (loop for (val . key) in *loop.6.alist.3*
	  do (setf (gethash key table) val))
    table))

;;; being {each | the} {hash-value | hash-values | hash-key | hash-keys} {in | of }

(deftest loop.6.1
  (loop for x being the hash-value of *loop.6.hash.1* sum x)
  6)

(deftest loop.6.2
  (loop for x being the hash-values of *loop.6.hash.1* sum x)
  6)

(deftest loop.6.3
  (loop for x being each hash-value of *loop.6.hash.1* sum x)
  6)

(deftest loop.6.4
  (loop for x being each hash-values of *loop.6.hash.1* sum x)
  6)

(deftest loop.6.5
  (loop for x being the hash-values in *loop.6.hash.1* sum x)
  6)

(deftest loop.6.6
  (sort (loop for x being the hash-key of *loop.6.hash.1* collect x)
	#'symbol<)
  (a b c))

(deftest loop.6.7
  (sort (loop for x being the hash-keys of *loop.6.hash.1* collect x)
	#'symbol<)
  (a b c))

(deftest loop.6.8
  (sort (loop for x being each hash-key of *loop.6.hash.1* collect x)
	#'symbol<)
  (a b c))

(deftest loop.6.9
  (sort (loop for x being each hash-keys of *loop.6.hash.1* collect x)
	#'symbol<)
  (a b c))

(deftest loop.6.10
  (sort (loop for x being each hash-keys in *loop.6.hash.1* collect x)
	#'symbol<)
  (a b c))

(deftest loop.6.11
  (sort (loop for (u . v) being the hash-keys of *loop.6.hash.3* collect u)
	#'symbol<)
  (a1 b1 c1))

(deftest loop.6.12
  (sort (loop for (u . v) being the hash-keys of *loop.6.hash.3* collect v)
	#'symbol<)
  (a2 b2 c2))

(deftest loop.6.13
  (sort (loop for (u . v) being the hash-values of *loop.6.hash.5* collect u)
	#'symbol<)
  (a1 b1 c1))

(deftest loop.6.14
  (sort (loop for (u . v) being the hash-values of *loop.6.hash.5* collect v)
	#'symbol<)
  (a2 b2 c2))

(deftest loop.6.15
  (sort (loop for k being the hash-keys of *loop.6.hash.1* using (hash-value v)
	      collect (list k v))
	#'< :key #'second)
  ((a 1) (b 2) (c 3)))

(deftest loop.6.16
  (sort (loop for v being the hash-values of *loop.6.hash.1* using (hash-key k)
	      collect (list k v))
	#'< :key #'second)
  ((a 1) (b 2) (c 3)))

(deftest loop.6.17
  (sort (loop for (u . nil) being the hash-values of *loop.6.hash.5* collect u)
	#'symbol<)
  (a1 b1 c1))

(deftest loop.6.18
  (sort (loop for (nil . v) being the hash-values of *loop.6.hash.5* collect v)
	#'symbol<)
  (a2 b2 c2))

(deftest loop.6.19
  (loop for nil being the hash-values of *loop.6.hash.5* count t)
  3)

(deftest loop.6.20
  (loop for nil being the hash-keys of *loop.6.hash.5* count t)
  3)

(deftest loop.6.21
  (loop for v being the hash-values of *loop.6.hash.5* using (hash-key nil) count t)
  3)

(deftest loop.6.22
  (loop for k being the hash-keys of *loop.6.hash.5* using (hash-value nil) count t)
  3)

(deftest loop.6.23
  (loop for v fixnum being the hash-values of *loop.6.hash.1* sum v)
  6)

(deftest loop.6.24
  (loop for v of-type fixnum being the hash-values of *loop.6.hash.1* sum v)
  6)


(deftest loop.6.25
  (loop for k fixnum being the hash-keys of *loop.6.hash.5* sum k)
  6)

(deftest loop.6.26
  (loop for k of-type fixnum being the hash-keys of *loop.6.hash.5* sum k)
  6)

