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
