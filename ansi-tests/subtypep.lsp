;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Jan 29 17:28:19 2003
;;;; Contains: Tests of SUBTYPEP

(in-package :cl-test)

;;; More subtypep tests are in types-and-class.lsp

(deftest simple-base-string-is-sequence
    (subtypep* 'simple-base-string 'sequence)
  t t)
(deftest subtype.env.1
  (mapcar #'notnot
	  (multiple-value-list (subtypep 'bit 'integer nil)))
  (t t))

(deftest subtype.env.2
  (macrolet
      ((%foo (&environment env)
	     (list 'quote
		   (mapcar #'notnot
			   (multiple-value-list
			    (subtypep 'bit 'integer env))))))
    (%foo))
  (t t))

(deftest subtype.env.3
  (macrolet
      ((%foo (&environment env)
	     (multiple-value-bind (sub good)
		 (subtypep nil (type-of env))
	       (or (not good) (notnot sub)))))
    (%foo))
  t)

(deftest subtype.env.4
  (macrolet
      ((%foo (&environment env)
	     (multiple-value-bind (sub good)
		 (subtypep (type-of env) (type-of env))
	       (or (not good) (notnot sub)))))
    (%foo))
  t)

(deftest subtype.env.5
  (macrolet
      ((%foo (&environment env)
	     (multiple-value-bind (sub good)
		 (subtypep (type-of env) t)
	       (or (not good) (notnot sub)))))
    (%foo))
  t)

(deftest subtypep.error.1
  (classify-error (subtypep))
  program-error)

(deftest subtypep.error.2
  (classify-error (subtypep t))
  program-error)

(deftest subtypep.error.3
  (classify-error (subtypep t t nil nil))
  program-error)

;;; Special cases of types-6 that are/were causing problems in CMU CL

(deftest keyword-is-subtype-of-atom
  (subtypep* 'keyword 'atom)
  t t)

(deftest ratio-is-subtype-of-atom
  (subtypep* 'ratio 'atom)
  t t)

(deftest extended-char-is-subtype-of-atom
  (subtypep* 'extended-char 'atom)
  t t)

(deftest string-is-not-simple-vector
  (subtypep* 'string 'simple-vector)
  nil t)

(deftest base-string-is-not-simple-vector
  (subtypep* 'base-string 'simple-vector)
  nil t)

(deftest simple-string-is-not-simple-vector
  (subtypep* 'simple-string 'simple-vector)
  nil t)

(deftest simple-base-string-is-not-simple-vector
  (subtypep* 'simple-base-string 'simple-vector)
  nil t)

(deftest bit-vector-is-not-simple-vector
  (subtypep* 'bit-vector 'simple-vector)
  nil t)

(deftest simple-bit-vector-is-not-simple-vector
  (subtypep* 'simple-bit-vector 'simple-vector)
  nil t)

(deftest subtypep.extended-char.1
  (if (subtypep* 'character 'base-char)
      (subtypep* 'extended-char nil)
    (values t t))
  t t)

(deftest subtypep.bignum.1
  (if (subtypep* 'integer 'fixnum)
      (subtypep* 'bignum nil)
    (values t t))
  t t)

(deftest subtypep.fixnum-or-bignum
  (check-equivalence '(or fixnum bignum) 'integer)
  nil)

(deftest subtypep.integer.1
  (subtypep* '(integer 0 10) '(integer 0 20))
  t t)

(deftest subtypep.integer.2
  (subtypep* '(integer 0 10) '(integer 0 (10)))
  nil t)

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
       (check-disjointness 'short-float 'single-float)
       (check-disjointness 'short-float 'double-float)
       (check-disjointness 'short-float 'long-float))
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
		   append (check-disjointness tp tp2))))
    nil)
  nil)

(deftest subtypep.float.5
  (if (and (not (subtypep 'single-float 'short-float))
	   (not (subtypep 'single-float 'double-float))
	   (subtypep 'double-float 'long-float))
      (append
       (check-disjointness 'short-float 'single-float)
       (check-disjointness 'short-float 'double-float)
       (check-disjointness 'short-float 'long-float)
       (check-disjointness 'single-float 'double-float)
       (check-disjointness 'single-float 'long-float)
       (check-equivalence 'double-float 'long-float))
    nil)
  nil)

(deftest subtypep.float.6
  (if (and (subtypep 'single-float 'short-float)
	   (not (subtypep 'single-float 'double-float))
	   (not (subtypep 'double-float 'long-float)))
      (append
       (check-equivalence 'short-float 'single-float)
       (check-disjointness 'single-float 'double-float)
       (check-disjointness 'single-float 'long-float)
       (check-disjointness 'double-float 'long-float))
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
		  append (check-disjointness tp tp2)))
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

