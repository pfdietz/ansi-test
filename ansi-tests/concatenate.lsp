;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Sep  4 22:53:51 2002
;;;; Contains: Tests for CONCATENATE

(in-package :cl-test)

(deftest concatenate.1
  (concatenate 'list)
  nil)

(deftest concatenate.2
  (let* ((orig (list 'a 'b 'c 'd 'e))
	 (copy (concatenate 'list orig)))
    (values
     copy
     (intersection (loop for e on orig collect e)
		   (loop for e on copy collect e)
		   :test #'eq)))
  (a b c d e)
  nil)

(deftest concatenate.3
  (concatenate 'list "")
  nil)

(deftest concatenate.4
  (concatenate 'list "abcd" '(x y z) nil #*1101 #())
  (#\a #\b #\c #\d x y z 1 1 0 1))

(deftest concatenate.5
  (concatenate 'vector)
  #())

(deftest concatenate.6
  (concatenate 'vector nil "abcd" '(x y z) nil #*1101 #())
  #(#\a #\b #\c #\d x y z 1 1 0 1))

(deftest concatenate.7
  (let* ((orig (vector 'a 'b 'c 'd 'e))
	 (copy (concatenate 'vector orig)))
    (values
     copy
     (eq copy orig)))
  #(a b c d e)
  nil)

(deftest concatenate.8
  (concatenate 'simple-vector '(a b c) #(1 2 3))
  #(a b c 1 2 3))

(deftest concatenate.9
  (concatenate 'simple-vector)
  #())

(deftest concatenate.10
  (concatenate 'bit-vector nil)
  #*)

(deftest concatenate.11
  (concatenate 'bit-vector)
  #*)

(deftest concatenate.12
  (concatenate 'bit-vector '(0 1 1) nil #(1 0 1) #())
  #*011101)

(deftest concatenate.13
  (concatenate 'simple-bit-vector nil)
  #*)

(deftest concatenate.14
  (concatenate 'simple-bit-vector)
  #*)

(deftest concatenate.15
  (concatenate 'simple-bit-vector '(0 1 1) nil #(1 0 1) #())
  #*011101)

(deftest concatenate.16
  (concatenate 'string "abc" '(#\d #\e) nil #() "fg")
  "abcdefg")

(deftest concatenate.17
  (concatenate 'simple-string "abc" '(#\d #\e) nil #() "fg")
  "abcdefg")

(deftest concatenate.18
  (concatenate '(vector * *) '(a b c) '(d e f) #(g h))
  #(a b c d e f g h))

(deftest concatenate.19
  (concatenate '(vector * 8) '(a b c) '(d e f) #(g h))
  #(a b c d e f g h))

(deftest concatenate.20
  (concatenate '(vector symbol 8) '(a b c) '(d e f) #(g h))
  #(a b c d e f g h))

(deftest concatenate.21
  (concatenate '(vector symbol) '(a b c) '(d e f) #(g h))
  #(a b c d e f g h))

(deftest concatenate.22
  (concatenate '(vector symbol *) '(a b c) '(d e f) #(g h))
  #(a b c d e f g h))

(deftest concatenate.23
  (concatenate 'cons '(a b c) '(d e f))
  (a b c d e f))

(deftest concatenate.24
  (concatenate 'null nil nil)
  nil)

;;; Error tests

(deftest concatenate-error.1
  (handler-case
   (concatenate 'sequence '(a b c))
   (error () :caught))
  :caught)

(deftest concatenate-error.2
  (handler-case  (concatenate 'fixnum '(a b c d e))
		 (error () :caught))
  :caught)

(deftest concatenate-error.3
  (catch-type-error (concatenate '(vector * 3) '(a b c d e)))
  type-error)
