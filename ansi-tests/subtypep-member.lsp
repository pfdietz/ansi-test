;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Feb 15 11:58:06 2003
;;;; Contains: Tests for subtype relationships on member types

(in-package :cl-test)

;;; SUBTYPEP on MEMBER types

(deftest subtypep.member.1
  (subtypep*-or-fail '(member a b c) '(member a b c d))
  t)

(deftest subtypep.member.2
  (subtypep*-not-or-fail '(member a b c) '(member a b))
  t)

(deftest subtypep.member.3
  (check-equivalence '(member) nil)
  nil)

(deftest subtypep.member.4
  (subtypep*-or-fail '(eql b) '(member a b c))
  t)

(deftest subtypep.member.5
  (subtypep*-or-fail '(member a b c d e) 'symbol)
  t)

(deftest subtypep.member.6
  (subtypep*-not-or-fail '(member a b 10 d e) 'symbol)
  t)

(deftest subtypep.member.7
  (subtypep*-or-fail 'null '(member a b nil c d e))
  t)

(deftest subtypep.member.8
  (subtypep*-not-or-fail 'null '(member a b c d e))
  t)

(deftest subtypep.member.9
  (let ((b1 (1+ most-positive-fixnum))
	(b2 (1+ most-positive-fixnum)))
    (subtypep*-or-fail `(member 10 ,b1 20) `(member 10 20 ,b2)))
  t)

(deftest subtypep.member.10
  (subtypep*-or-fail '(member :a :b :c) 'keyword)
  t)

(deftest subtypep.member.11
  (let ((b1 (copy-list '(a)))
	(b2 (copy-list '(a))))
    (subtypep*-not-or-fail `(member 10 ,b1 20) `(member 10 20 ,b2)))
  t)

(deftest subtypep.member.12
  (let ((b1 '(a)))
    (subtypep*-or-fail `(member 10 ,b1 20) `(member 10 20 ,b1)))
  t)

(deftest subtypep.member.13
  (subtypep*-or-fail '(member 10 20 30) '(integer 0 100))
  t)

(deftest subtypep.member.14
  (subtypep*-or-fail '(integer 3 6) '(member 0 1 2 3 4 5 6 7 8 100))
  t)

(deftest subtypep.member.15
  (subtypep*-not-or-fail '(integer 3 6) '(member 0 1 2 3 5 6 7 8))
  t)

(deftest subtypep.member.16
  (check-equivalence '(integer 2 5) '(member 2 5 4 3))
  nil)

(deftest subtypep.member.17
  (let ((s1 (copy-seq "abc"))
	(s2 (copy-seq "abc")))
    (let ((t1 `(member ,s1))
	  (t2 `(member ,s2)))
      (cond
       ((subtypep t1 t2) "T1 is subtype of T2")
       ((subtypep t2 t1) "T2 is subtype of T1")
       (t (check-disjointness t1 t2)))))
  nil)

(deftest subtypep.member.18
  (let ((s1 (copy-seq '(a b c)))
	(s2 (copy-seq '(a b c))))
    (let ((t1 `(member ,s1))
	  (t2 `(member ,s2)))
      (cond
       ((subtypep t1 t2) "T1 is subtype of T2")
       ((subtypep t2 t1) "T2 is subtype of T1")
       (t (check-disjointness t1 t2)))))
  nil)

(deftest subtypep.member.19
  (let ((i1 (1+ most-positive-fixnum))
	(i2 (1+ most-positive-fixnum)))
    (check-equivalence `(member 0 ,i1) `(member 0 ,i2)))
  nil)

(deftest subtypep.member.20
  (check-equivalence '(and (member a b c d) (member e d b f g))
		     '(member b d))
  nil)

(deftest subtypep.member.21
  (check-equivalence '(and (member a b c d) (member e d f g))
		     '(eql d))
  nil)

(deftest subtypep.member.22
  (check-equivalence '(and (member a b c d) (member e f g))
		     nil)
  nil)

(deftest subtypep.member.23
  (check-equivalence '(or (member a b c) (member z b w))
		     '(member z a b w c))
  nil)

(deftest subtypep.member.24
  (check-equivalence '(or (member a b c) (eql d))
		     '(member d c b a))
  nil)

(deftest subtypep.member.25
  (check-equivalence 'boolean '(member nil t))
  nil)

(deftest subtypep.member.26
  (check-equivalence '(or (eql a) (eql b))
		     '(member a b))
  nil)

