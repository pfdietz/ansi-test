;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Nov  2 13:52:50 2002
;;;; Contains: Tests of LOOP clause FOR-AS-ACROSS

(in-package :cl-test)

(deftest loop.5.1
  (let ((x "abcd")) (loop for e across x collect e))
  (#\a #\b #\c #\d))

(deftest loop.5.2
  (let ((x "abcd")) (loop for e across (the string x) collect e))
  (#\a #\b #\c #\d))

(deftest loop.5.3
  (let ((x "abcd")) (loop for e across (the simple-string x) collect e))
  (#\a #\b #\c #\d))

(deftest loop.5.4
  (loop for e across "abcd" collect e)
  (#\a #\b #\c #\d))

(deftest loop.5.5
  (loop for e across "abcd"
	for i from 1 to 3 collect e)
  (#\a #\b #\c))

(deftest loop.5.6
  (loop for e of-type base-char across "abcd"
	for i from 1 to 3 collect e)
  (#\a #\b #\c))

(deftest loop.5.7
  (let ((x "abcd")) (loop for e across (the base-string x) collect e))
  (#\a #\b #\c #\d))

(deftest loop.5.8
  (let ((x "abcd")) (loop for e of-type character across x collect e))
  (#\a #\b #\c #\d))


(deftest loop.5.10
  (let ((x #*00010110))
    (loop for e across x collect e))
  (0 0 0 1 0 1 1 0))

(deftest loop.5.11
  (let ((x #*00010110))
    (loop for e across (the bit-vector x) collect e))
  (0 0 0 1 0 1 1 0))

(deftest loop.5.12
  (let ((x #*00010110))
    (loop for e across (the simple-bit-vector x) collect e))
  (0 0 0 1 0 1 1 0))

(deftest loop.5.13
  (let ((x #*00010110))
    (loop for e of-type bit across (the simple-bit-vector x) collect e))
  (0 0 0 1 0 1 1 0))

(deftest loop.5.14
  (let ((x #*00010110))
    (loop for e of-type bit across x
	  for i from 1 to 4 collect e))
  (0 0 0 1))


(deftest loop.5.20
  (let ((x (vector 'a 'b 'c 'd)))
    (loop for e across x collect e))
  (a b c d))

(deftest loop.5.21
  (let ((x (vector 'a 'b 'c 'd)))
    (loop for e across (the vector x) collect e))
  (a b c d))

(deftest loop.5.22
  (let ((x (vector 'a 'b 'c 'd)))
    (loop for e across (the simple-vector x) collect e))
  (a b c d))

(deftest loop.5.23
  (let ((x (vector '(a) '(b) '(c) '(d))))
    (loop for (e) across x collect e))
  (a b c d))


(deftest loop.5.30
  (let ((x (make-array '(5) :initial-contents '(a b c d e)
		  :adjustable t)))
    (loop for e across x collect e))
  (a b c d e))

(deftest loop.5.32
  (let* ((x (make-array '(5) :initial-contents '(a b c d e)))
	 (y (make-array '(3) :displaced-to x
		   :displaced-index-offset 1)))
    (loop for e across y collect e))
  (b c d))

;;; tests of 'as' form

(deftest loop.5.33
  (loop as e across "abc" collect e)
  (#\a #\b #\c))

(deftest loop.5.34
  (loop as e of-type character across "abc" collect e)
  (#\a #\b #\c))

(deftest loop.5.35
  (loop as e of-type integer across (the simple-vector (coerce '(1 2 3) 'simple-vector))
	sum e)
  6)

;;; Loop across displaced vectors

(deftest loop.5.36
  (let* ((a (make-array '(10) :initial-contents '(a b c d e f g h i j)))
	 (da (make-array '(5) :displaced-to a
			 :displaced-index-offset 2)))
    (loop for e across da collect e))
  (c d e f g))

(deftest loop.5.37
  (let* ((a (make-array '(10) :element-type 'base-char
			:initial-contents "abcdefghij"))
	 (da (make-array '(5) :element-type 'base-char
			 :displaced-to a
			 :displaced-index-offset 2)))
    (loop for e across da collect e))
  (#\c #\d #\e #\f #\g))

(deftest loop.5.38
  (let* ((a (make-array '(10) :element-type 'bit
			:initial-contents '(0 1 1 0 0 1 0 1 1 1)))
	 (da (make-array '(5) :element-type 'bit
			 :displaced-to a
			 :displaced-index-offset 2)))
    (loop for e across da collect e))
  (1 0 0 1 0))


;;; Error cases

(deftest loop.5.error.1
  (classify-error
   (loop for (e . e) across (vector '(x . y) '(u . v)) collect e))
  program-error)

(deftest loop.5.error.2
  (classify-error
   (loop for e across (vector '(x . y) '(u . v))
	 for e from 1 to 5 collect e))
  program-error)

(deftest loop.5.error.3
  (classify-error*
   (macroexpand 
    '(loop for (e . e) across (vector '(x . y) '(u . v)) collect e)))
  program-error)

(deftest loop.5.error.4
  (classify-error*
   (macroexpand
    '(loop for e across (vector '(x . y) '(u . v))
	   for e from 1 to 5 collect e)))
  program-error)

