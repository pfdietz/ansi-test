;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Feb 15 11:55:37 2003
;;;; Contains: Tests for subtype relationships on float types

(in-package :cl-test)

;;;;;;;

(deftest subtypep.float.1
  (loop for tp in +float-types+
	append (check-subtypep tp 'float t t))
  nil)

(deftest subtypep.float.2
  (if (subtypep 'short-float 'long-float)
      (loop for tp in +float-types+
	    append
	    (loop for tp2 in +float-types+
		  append (check-subtypep tp tp2 t t)))
    nil)
  nil)

(deftest subtypep.float.3
  (if (and (not (subtypep 'short-float 'single-float))
	   (subtypep 'single-float 'long-float))
      (append
       (check-equivalence 'single-float 'double-float)
       (check-equivalence 'single-float 'long-float)
       (check-equivalence 'double-float 'long-float)
       (classes-are-disjoint 'short-float 'single-float)
       (classes-are-disjoint 'short-float 'double-float)
       (classes-are-disjoint 'short-float 'long-float))
    nil)
  nil)

(deftest subtypep.float.4
  (if (and (subtypep 'single-float 'short-float)
	   (subtypep 'double-float 'long-float)
	   (not (subtypep 'short-float 'double-float)))
      (append
       (check-equivalence 'short-float 'single-float)
       (check-equivalence 'double-float 'long-float)
       (loop for tp in '(short-float single-float)
	     append
	     (loop for tp2 in '(double-float long-float)
		   append (classes-are-disjoint tp tp2))))
    nil)
  nil)

(deftest subtypep.float.5
  (if (and (not (subtypep 'single-float 'short-float))
	   (not (subtypep 'single-float 'double-float))
	   (subtypep 'double-float 'long-float))
      (append
       (classes-are-disjoint 'short-float 'single-float)
       (classes-are-disjoint 'short-float 'double-float)
       (classes-are-disjoint 'short-float 'long-float)
       (classes-are-disjoint 'single-float 'double-float)
       (classes-are-disjoint 'single-float 'long-float)
       (check-equivalence 'double-float 'long-float))
    nil)
  nil)

(deftest subtypep.float.6
  (if (and (subtypep 'single-float 'short-float)
	   (not (subtypep 'single-float 'double-float))
	   (not (subtypep 'double-float 'long-float)))
      (append
       (check-equivalence 'short-float 'single-float)
       (classes-are-disjoint 'single-float 'double-float)
       (classes-are-disjoint 'single-float 'long-float)
       (classes-are-disjoint 'double-float 'long-float))
    nil)
  nil)

(deftest subtypep.float.7
  (if (and (not (subtypep 'single-float 'short-float))
	   (not (subtypep 'single-float 'double-float))
	   (not (subtypep 'double-float 'long-float)))
      (loop for tp in +float-types+
	    append
	    (loop for tp2 in +float-types+
		  unless (eq tp tp2)
		  append (classes-are-disjoint tp tp2)))
    nil)
  nil)

(deftest subtypep.float.8
  (subtypep* '(short-float 0.0s0 10.0s0) '(short-float 0.0s0 11.0s0))
  t t)

(deftest subtypep.float.9
  (subtypep* '(single-float 0.0f0 10.0f0) '(single-float 0.0f0 11.0f0))
  t t)

(deftest subtypep.float.10
  (subtypep* '(double-float 0.0d0 10.0d0) '(double-float 0.0d0 11.0d0))
  t t)

(deftest subtypep.float.11
  (subtypep* '(long-float 0.0l0 10.0l0) '(long-float 0.0l0 11.0l0))
  t t)

(deftest subtypep.float.12
  (subtypep* '(short-float 0.0s0 11.0s0) '(short-float 0.0s0 10.0s0))
  nil t)

(deftest subtypep.float.13
  (subtypep* '(single-float 0.0f0 11.0f0) '(single-float 0.0f0 10.0f0))
  nil t)

(deftest subtypep.float.14
  (subtypep* '(double-float 0.0d0 11.0d0) '(double-float 0.0d0 10.0d0))
  nil t)

(deftest subtypep.float.15
  (subtypep* '(long-float 0.0l0 11.0l0) '(long-float 0.0l0 10.0l0))
  nil t)

(deftest subtypep.float.16
  (subtypep* '(short-float 0.0s0 (10.0s0)) '(short-float 0.0s0 10.0s0))
  t t)

(deftest subtypep.float.17
  (subtypep* '(single-float 0.0f0 (10.0f0)) '(single-float 0.0f0 10.0f0))
  t t)

(deftest subtypep.float.18
  (subtypep* '(double-float 0.0d0 (10.0d0)) '(double-float 0.0d0 10.0d0))
  t t)

(deftest subtypep.float.19
  (subtypep* '(long-float 0.0l0 (10.0l0)) '(long-float 0.0l0 10.0l0))
  t t)

(deftest subtypep.float.20
  (subtypep* '(short-float 0.0s0 10.0s0) '(short-float 0.0s0 (10.0s0)))
  nil t)

(deftest subtypep.float.21
  (subtypep* '(single-float 0.0f0 10.0f0) '(single-float 0.0f0 (10.0f0)))
  nil t)

(deftest subtypep.float.22
  (subtypep* '(double-float 0.0d0 10.0d0) '(double-float 0.0d0 (10.0d0)))
  nil t)

(deftest subtypep.float.23
  (subtypep* '(long-float 0.0l0 10.0l0) '(long-float 0.0l0 (10.0l0)))
  nil t)

(deftest subtypep.float.24
  (check-equivalence '(and (short-float 0.0s0 2.0s0)
			   (short-float 1.0s0 3.0s0))
		     '(short-float 1.0s0 2.0s0))
  nil)

(deftest subtypep.float.25
  (check-equivalence '(and (single-float 0.0f0 2.0f0)
			   (single-float 1.0f0 3.0f0))
		     '(single-float 1.0f0 2.0f0))
  nil)

(deftest subtypep.float.26
  (check-equivalence '(and (double-float 0.0d0 2.0d0)
			   (double-float 1.0d0 3.0d0))
		     '(double-float 1.0d0 2.0d0))
  nil)

(deftest subtypep.float.27
  (check-equivalence '(and (long-float 0.0l0 2.0l0)
			   (long-float 1.0l0 3.0l0))
		     '(long-float 1.0l0 2.0l0))
  nil)
