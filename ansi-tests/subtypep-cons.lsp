;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Feb 15 11:57:03 2003
;;;; Contains: Tests for subtype relationships on cons types

(in-package :cl-test)

;;; SUBTYPEP on CONS types

(defvar *cons-types*
  '(cons (cons) (cons *) (cons * *) (cons t) (cons t t)
	 (cons t *) (cons * t)))

(deftest subtypep.cons.1
  (loop for t1 in *cons-types*
	append (loop for t2 in *cons-types*
		     unless (equal (mapcar #'notnot
					   (multiple-value-list
					    (subtypep t1 t2)))
				   '(t t))
		     collect (list t1 t2)))
  nil)

(deftest subtypep.cons.2
  (loop for t1 in '((cons nil) (cons nil *) (cons nil t)
		    (cons * nil) (cons t nil) (cons nil nil))
	unless (subtypep t1 nil)
	collect t1)
  nil)

(deftest subtypep.cons.3
  (check-equivalence '(and (cons symbol *) (cons * symbol))
		     '(cons symbol symbol))
  nil)

(deftest subtypep.cons.4
  (check-equivalence '(and (cons (integer 0 10) *)
			   (cons (integer 5 15) (integer 10 20))
			   (cons * (integer 15 25)))
		     '(cons (integer 5 10) (integer 15 20)))
  nil)

(deftest subtypep.cons.5
  (check-equivalence
   '(and cons (not (cons symbol symbol)))
   '(or (cons (not symbol) *)
	(cons * (not symbol))))
  nil)

(deftest subtypep.cons.6
  (check-equivalence
   '(or (cons integer symbol) (cons integer integer)
	(cons symbol integer) (cons symbol symbol))
   '(cons (or integer symbol) (or integer symbol)))
  nil)

(deftest subtypep.cons.7
  (check-equivalence
   '(or (cons (integer 0 8) (integer 5 15))
	(cons (integer 0 7) (integer 0 6))
	(cons (integer 6 15) (integer 0 9))
	(cons (integer 3 15) (integer 4 15)))
   '(cons (integer 0 15) (integer 0 15)))
  nil)

(deftest subtypep.cons.8
  (check-equivalence
   '(or
     (cons integer (cons symbol integer))
     (cons symbol (cons integer symbol))
     (cons symbol (cons symbol integer))
     (cons symbol (cons integer integer))
     (cons integer (cons integer symbol))
     (cons symbol (cons symbol symbol))
     (cons integer (cons integer integer))
     (cons integer (cons symbol symbol)))
   '(cons (or symbol integer)
	  (cons (or symbol integer) (or symbol integer))))
  nil)

(deftest subtypep.cons.9
  (check-equivalence
   '(or
     (cons (integer 0 (3)) (integer 0 (6)))
     (cons (integer 3 (9)) (integer 0 (3)))
     (cons (integer 0 (6)) (integer 6 (9)))
     (cons (integer 6 (9)) (integer 3 (9)))
     (cons (integer 3 (6)) (integer 3 (6))))
   '(cons (integer 0 (9)) (integer 0 (9))))
  nil)

(deftest subtypep.cons.10
  (check-equivalence
   '(or
     (cons (rational 0 (3)) (rational 0 (6)))
     (cons (rational 3 (9)) (rational 0 (3)))
     (cons (rational 0 (6)) (rational 6 (9)))
     (cons (rational 6 (9)) (rational 3 (9)))
     (cons (rational 3 (6)) (rational 3 (6))))
   '(cons (rational 0 (9)) (rational 0 (9))))
  nil)

(deftest subtypep.cons.11
  (check-equivalence
   '(or
     (cons (real 0 (3)) (real 0 (6)))
     (cons (real 3 (9)) (real 0 (3)))
     (cons (real 0 (6)) (real 6 (9)))
     (cons (real 6 (9)) (real 3 (9)))
     (cons (real 3 (6)) (real 3 (6))))
   '(cons (real 0 (9)) (real 0 (9))))
  nil)

;;; Test suggested by C.R.
(deftest subtypep.cons.12
  (check-all-not-subtypep
   '(cons (or integer symbol)
	  (or integer symbol))
   '(or (cons integer symbol)
	(cons symbol integer)))
  nil)

(deftest subtypep.cons.13
  (check-all-not-subtypep '(not list) 'cons)
  nil)


;;; a -> b, a ==> b
(deftest subtypep.cons.14
  (check-all-subtypep
   '(and (or (cons (not symbol)) (cons * integer))
	 (cons symbol))
   '(cons * integer))
  nil)

;;; a -> b, not b ==> not a
(deftest subtypep.cons.15
  (check-all-subtypep
   '(and (or (cons (not symbol)) (cons * integer))
	 (cons * (not integer)))
   '(cons (not symbol)))
  nil)

;;; (and (or a b) (or (not b) c)) ==> (or a c)
(deftest subtypep.cons.16
  (check-all-subtypep
   '(and (or (cons symbol (cons * *))
	     (cons * (cons integer *)))
	 (or (cons * (cons (not integer) *))
	     (cons * (cons * float))))
   '(or (cons symbol (cons * *))
	(cons * (cons * float))))
  nil)

(deftest subtypep.cons.17
  (check-all-subtypep
   '(and (or (cons symbol (cons * *))
	     (cons * (cons integer *)))
	 (or (cons * (cons (not integer)))
	     (cons * (cons * float)))
	 (or (cons * (cons * (not float)))
	     (cons symbol (cons * *))))
   '(cons symbol))
  nil)

(deftest subtypep.cons.18
  (check-all-subtypep
   '(cons symbol)
   '(or (cons symbol (not integer))
	(cons * integer)))
  nil)

(deftest subtypep.cons.19
  (check-equivalence
   '(or
     (cons (eql a) (eql x))
     (cons (eql b) (eql y))
     (cons (eql c) (eql z))
     (cons (eql a) (eql y))
     (cons (eql b) (eql z))
     (cons (eql c) (eql x))
     (cons (eql a) (eql z))
     (cons (eql b) (eql x))
     (cons (eql c) (eql y)))
   '(cons (member a b c) (member x y z)))
  nil)

(deftest subtypep.cons.20
  (check-equivalence
   '(or
     (cons (eql a) (eql x))
     (cons (eql b) (eql y))
     (cons (eql a) (eql y))
     (cons (eql b) (eql z))
     (cons (eql c) (eql x))
     (cons (eql a) (eql z))
     (cons (eql b) (eql x))
     (cons (eql c) (eql y)))
   '(and (cons (member a b c) (member x y z))
	 (not (cons (eql c) (eql z)))))
  nil)

