;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Nov  2 21:38:08 2002
;;;; Contains: Tests for COPY-SEQ

(in-package :cl-test)

;;; This function is extensively used elsewhere, but is tested again
;;; here for completeness.

(deftest copy-seq.1
  (copy-seq nil)
  nil)

(deftest copy-seq.2
  (let* ((s1 '(a b c))
	 (s2 (check-values (copy-seq s1))))
    (and (not (eql s1 s2))
	 (equalt s1 s2)))
  t)

(deftest copy-seq.3
  (let* ((s1 #(a b c))
	 (s2 (check-values (copy-seq s1))))
    (and (not (eql s1 s2)) s2))
  #(a b c))

(deftest copy-seq.4
  (let* ((s1 (make-array '(4) :initial-contents '(a b c d)
			 :adjustable t))
	 (s2 (check-values (copy-seq s1))))
    (and (not (eql s1 s2))
	 (simple-vector-p s2)
	 s2))
  #(a b c d))


(deftest copy-seq.5
  (let* ((s1 (make-array '(4) :initial-contents '(a b c d)
			 :fill-pointer 3))
	 (s2 (check-values (copy-seq s1))))
    (and (not (eql s1 s2))
	 (simple-vector-p s2)
	 s2))
  #(a b c))

(deftest copy-seq.6
  (let* ((a1 (make-array '(6) :initial-contents '(a b c d e f)))
	 (a2 (make-array '(4) :displaced-to a1
			 :displaced-index-offset 1))
	 (s2 (check-values (copy-seq a2))))
    (and (not (eql a2 s2))
	 (simple-vector-p s2)
	 s2))
  #(b c d e))

(deftest copy-seq.7
  (let* ((s1 (make-array '(4)
			 :element-type 'base-char
			 :initial-contents '(#\a #\b #\c #\d)
			 :adjustable t))
	 (s2 (check-values (copy-seq s1))))
    (and (not (eql s1 s2))
	 (simple-string-p s2)
	 s2))
  "abcd")


(deftest copy-seq.8
  (let* ((s1 (make-array '(4)
			 :element-type 'base-char
			 :initial-contents '(#\a #\b #\c #\d)
			 :fill-pointer 3))
	 (s2 (check-values (copy-seq s1))))
    (and (not (eql s1 s2))
	 (simple-string-p s2)
	 s2))
  "abc")

(deftest copy-seq.9
  (let* ((a1 (make-array '(6) :initial-contents '(#\a #\b #\c #\d #\e #\f)
			 :element-type 'base-char))
	 (a2 (make-array '(4) :displaced-to a1
			 :element-type 'base-char
			 :displaced-index-offset 1))
	 (s2 (check-values (copy-seq a2))))
    (and (not (eql a2 s2))
	 (simple-string-p s2)
	 s2))
  "bcde")

(deftest copy-seq.10
  (let*((s1 "abcd")
	(s2 (check-values (copy-seq s1))))
    (and (not (eql s1 s2))
	 s2))
  "abcd")

(deftest copy-seq.11
  (let* ((s1 #*0010110)
	 (s2 (check-values (copy-seq s1))))
    (and (not (eql s1 s2))
	 (simple-bit-vector-p s2)
	 s2))
  #*0010110)

(deftest copy-seq.12
  (let* ((s1 (make-array '(4) :initial-contents '(0 0 1 0)
			 :element-type 'bit
			 :adjustable t))
	 (s2 (check-values (copy-seq s1))))
    (and (not (eql s1 s2))
	 (simple-bit-vector-p s2)
	 s2))
  #*0010)

(deftest copy-seq.13
  (let* ((s1 (make-array '(4) :initial-contents '(0 0 1 0)
			 :element-type 'bit
			 :fill-pointer 3))
	 (s2 (check-values (copy-seq s1))))
    (and (not (eql s1 s2))
	 (simple-bit-vector-p s2)
	 s2))
  #*001)

(deftest copy-seq.14
  (let* ((a1 (make-array '(6) :initial-contents '(0 0 1 0 1 1)
			 :element-type 'bit))
	 (a2 (make-array '(4) :displaced-to a1
			 :displaced-index-offset 1
			 :element-type 'bit))
	 (s2 (check-values (copy-seq a2))))
    (and (not (eql a2 s2))
	 (simple-bit-vector-p s2)
	 s2))
  #*0101)

(deftest copy-seq.15
  (copy-seq "")
  "")

(deftest copy-seq.16
  (copy-seq #*)
  #*)

(deftest copy-seq.17
  (copy-seq #())
  #())

(deftest copy-seq.18
  (let* ((x (make-array '(10) :initial-contents '(a b c d e f g h i j)))
	 (y (check-values (copy-seq x))))
    (equal-array x y))
  t)

(deftest copy-seq.order.1
  (let ((i 0))
    (values (copy-seq (progn (incf i) "abc")) i))
  "abc" 1)

;;; Error tests

(deftest copy-seq.error.1
  (classify-error (copy-seq 10))
  type-error)

(deftest copy-seq.error.2
  (classify-error (copy-seq 'a))
  type-error)

(deftest copy-seq.error.3
  (classify-error (copy-seq 13.21))
  type-error)

(deftest copy-seq.error.4
  (classify-error (copy-seq))
  program-error)

(deftest copy-seq.error.5
  (classify-error (copy-seq "abc" 2 nil))
  program-error)

(deftest copy-seq.error.6
  (classify-error (locally (copy-seq 10) t))
  type-error)

