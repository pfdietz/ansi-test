;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar  1 16:23:57 2003
;;;; Contains: Tests of SUBTYPEP on array types

(in-package :cl-test)

;;; *array-element-types* is defined in ansi-aux.lsp

(deftest subtypep.array.1
  (let ((array-types (cons (find-class 'array)
			   '(array (array) (array *) (array * *)))))
    (loop for tp1 in array-types append
	  (loop for tp2 in array-types
		unless (subtypep tp1 tp2)
		collect (list tp1 tp2))))
  nil)

(deftest subtypep.array.2
  (and (subtypep* '(array t) '(array t *))
       (subtypep* '(array t *) '(array t))
       t)
  t)

(deftest subtypep.array.3
  (loop for i from 0 below (min 16 array-rank-limit)
	for type = `(array * ,i)
	for type2 = `(array * ,(make-list i :initial-element '*))
	unless (and (subtypep type 'array)
		    (subtypep type '(array))
		    (subtypep type '(array *))
		    (subtypep type '(array * *))
		    (subtypep type type2))
	collect type)
  nil)

(deftest subtypep.array.4
  (loop for i from 0 below (min 16 array-rank-limit)
	for type = `(array t ,i)
	for type2 = `(array t ,(make-list i :initial-element '*))
	unless (and (subtypep type '(array t))
		    (subtypep type '(array t *))
		    (subtypep type type2))
	collect type)
  nil)

(deftest subtypep.array.5
  (loop
   for element-type in (cons '* *array-element-types*)
   nconc
   (loop for i from 0 below (min 16 array-rank-limit)
	 for type = `(array ,element-type ,i)
	 for type2 = `(array ,element-type ,(make-list i :initial-element '0))
	 for type3 = `(array ,element-type ,(make-list i :initial-element '1))
	 unless
	 (and (subtypep type2 type)
	      (subtypep type3 type)
	      (loop for j from 0 to i
		    always
		    (and
		     (subtypep
		      `(array ,element-type
			      (,@(make-list j :initial-element '*)
				 ,@(make-list (- i j) :initial-element 2)))
		      type)
		     (subtypep
		      `(array ,element-type
			      (,@(make-list j :initial-element 2)
				 ,@(make-list (- i j) :initial-element '*)))
		      type))))   
	 collect type))
  nil)

(deftest subtypep.array.6
  (loop
   for etype in (cons '* *array-element-types*)
   append
   (check-equivalence
    `(and (array ,etype (* 10 * * *))
	  (array ,etype (* * * 29 *)))
    `(array ,etype (* 10 * 29 *))))
  nil)

(deftest subtypep.array.7
  (let ((etypes *array-element-types*))
    (loop
     for etp1 in etypes
     for uaetp1 = (upgraded-array-element-type etp1)
     append
     (loop for etp2 in etypes
	   for uaetp2 = (upgraded-array-element-type etp2)
	   when (equal (multiple-value-list (subtypep* uaetp1 uaetp2))
		       '(nil t))
	   append (check-disjointness `(array ,etp1) `(array ,etp2)))))
  nil)

		 

;;;; Tests on the definitions of various vector types

(deftest string-is-vector-of-character.1
  (subtypep* 'string '(vector character))
  t t)

(deftest string-is-vector-of-character.2
  (subtypep* '(vector character) 'string)
  t t)

(deftest string-is-vector-of-character.3
  (subtypep* '(string *) '(vector character))
  t t)

(deftest string-is-vector-of-character.4
  (subtypep* '(vector character) '(string *))
  t t)

(deftest string-is-vector-of-character.5
  (subtypep* '(string 17) '(vector character 17))
  t t)

(deftest string-is-vector-of-character.6
  (subtypep* '(vector character 17) '(string 17))
  t t)

(deftest base-string-is-vector-of-base-char.1
  (subtypep* 'base-string '(vector base-char))
  t t)

(deftest base-string-is-vector-of-base-char.2
  (subtypep* '(vector base-char) 'base-string)
  t t)

(deftest base-string-is-vector-of-base-char.3
  (subtypep* '(base-string *) '(vector base-char))
  t t)

(deftest base-string-is-vector-of-base-char.4
  (subtypep* '(vector base-char) '(base-string *))
  t t)

(deftest base-string-is-vector-of-base-char.5
  (subtypep* '(base-string 17) '(vector base-char 17))
  t t)

(deftest base-string-is-vector-of-base-char.6
  (subtypep* '(vector base-char 17) '(base-string 17))
  t t)

(deftest simple-base-string-is-simple-1d-array-of-base-char.1
  (subtypep* 'simple-base-string '(simple-array base-char (*)))
  t t)

(deftest simple-base-string-is-simple-1d-array-of-base-char.2
  (subtypep* '(simple-array base-char (*)) 'simple-base-string)
  t t)

(deftest simple-base-string-is-simple-1d-array-of-base-char.3
  (subtypep* '(simple-base-string *) '(simple-array base-char (*)))
  t t)

(deftest simple-base-string-is-simple-1d-array-of-base-char.4
  (subtypep* '(simple-array base-char (*)) '(simple-base-string *))
  t t)

(deftest simple-base-string-is-simple-1d-array-of-base-char.5
  (subtypep* '(simple-base-string 17) '(simple-array base-char (17)))
  t t)

(deftest simple-base-string-is-simple-1d-array-of-base-char.6
  (subtypep* '(simple-array base-char (17)) '(simple-base-string 17))
  t t)

(deftest simple-string-is-simple-1d-array-of-character.1
  (subtypep* 'simple-string '(simple-array character (*)))
  t t)

(deftest simple-string-is-simple-1d-array-of-character.2
  (subtypep* '(simple-array character (*)) 'simple-string)
  t t)

(deftest simple-string-is-simple-1d-array-of-character.3
  (subtypep* '(simple-string *) '(simple-array character (*)))
  t t)

(deftest simple-string-is-simple-1d-array-of-character.4
  (subtypep* '(simple-array character (*)) '(simple-string *))
  t t)

(deftest simple-string-is-simple-1d-array-of-character.5
  (subtypep* '(simple-string 17) '(simple-array character (17)))
  nil t)

(deftest simple-string-is-simple-1d-array-of-character.6
  (subtypep* '(simple-array character (17)) '(simple-string 17))
  t t)

(deftest vector-is-1d-array.1
  (subtypep* 'vector '(array * (*)))
  t t)

(deftest vector-is-1d-array.2
  (subtypep* '(array * (*)) 'vector)
  t t)

(deftest vector-is-1d-array.3
  (subtypep* '(vector *) '(array * (*)))
  t t)

(deftest vector-is-1d-array.4
  (subtypep* '(array * (*)) '(vector *))
  t t)

(deftest vector-is-1d-array.5
  (subtypep* '(vector * 17) '(array * (17)))
  t t)

(deftest vector-is-1d-array.6
  (subtypep* '(array * (17)) '(vector * 17))
  t t)

(deftest simple-vector-is-simple-1d-array.1
  (subtypep* 'simple-vector '(simple-array t (*)))
  t t)

(deftest simple-vector-is-simple-1d-array.2
  (subtypep* '(simple-array t (*)) 'simple-vector)
  t t)

(deftest simple-vector-is-simple-1d-array.3
  (subtypep* '(simple-vector *) '(simple-array t (*)))
  t t)

(deftest simple-vector-is-simple-1d-array.4
  (subtypep* '(simple-array t (*)) '(simple-vector *))
  t t)

(deftest simple-vector-is-simple-1d-array.5
  (subtypep* '(simple-vector 17) '(simple-array t (17)))
  t t)

(deftest simple-vector-is-simple-1d-array.6
  (subtypep* '(simple-array t (17)) '(simple-vector 17))
  t t)
