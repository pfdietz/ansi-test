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

