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

;;; (deftest subtypep.bignum.1
;;;   (if (subtypep* 'integer 'fixnum)
;;;      (subtypep* 'bignum nil)
;;;    (values t t))
;;;  t t)

(deftest subtypep.fixnum-or-bignum
  (check-equivalence '(or fixnum bignum) 'integer)
  nil)

(deftest subtypep.fixnum.integer
  (check-equivalence `(integer ,most-negative-fixnum ,most-positive-fixnum)
		     'fixnum)
  nil)

(deftest subtypep.bignum.integer
  (check-equivalence
   `(or (integer * (,most-negative-fixnum))
	(integer (,most-positive-fixnum) *))
   'bignum)
  nil)

;;;;;;;

(deftest subtypep.integer.1
  (subtypep* '(integer 0 10) '(integer 0 20))
  t t)

(deftest subtypep.integer.2
  (subtypep* '(integer 0 10) '(integer 0 (10)))
  nil t)

(deftest subtypep.integer.3
  (subtypep* '(integer 10 100) 'integer)
  t t)

(deftest subtypep.integer.4
  (subtypep* 'integer '(integer 10 100))
  nil t)

(deftest subtypep.integer.5
  (subtypep* '(integer 10 *) 'integer)
  t t)

(deftest subtypep.integer.6
  (subtypep* 'integer '(integer 10 *))
  nil t)

(deftest subtypep.integer.7
  (subtypep* '(integer 10) 'integer)
  t t)

(deftest subtypep.integer.8
  (subtypep* 'integer '(integer 10))
  nil t)

(deftest subtypep.integer.9
  (subtypep* '(integer * 10) 'integer)
  t t)

(deftest subtypep.integer.10
  (subtypep* 'integer '(integer * 10))
  nil t)

(deftest subtypep.integer.11
  (subtypep* '(integer 10) '(integer 5))
  t t)

(deftest subtypep.integer.12
  (subtypep* '(integer 5) '(integer 10))
  nil t)

(deftest subtypep.integer.13
  (subtypep* '(integer 10 *) '(integer 5))
  t t)

(deftest subtypep.integer.14
  (subtypep* '(integer 5) '(integer 10 *))
  nil t)

(deftest subtypep.integer.15
  (subtypep* '(integer 10) '(integer 5 *))
  t t)

(deftest subtypep.integer.16
  (subtypep* '(integer 5 *) '(integer 10))
  nil t)

(deftest subtypep.integer.17
  (subtypep* '(integer 10 *) '(integer 5 *))
  t t)

(deftest subtypep.integer.18
  (subtypep* '(integer 5 *) '(integer 10 *))
  nil t)

(deftest subtypep.integer.19
  (subtypep* '(integer * 5) '(integer * 10))
  t t)

(deftest subtypep.integer.20
  (subtypep* '(integer * 10) '(integer * 5))
  nil t)

(deftest subtypep.integer.21
  (subtypep* '(integer 10 *) '(integer * 10))
  nil t)

(deftest subtypep.integer.22
  (subtypep* '(integer * 10) '(integer 10 *))
  nil t)

(deftest subtypep.integer.23
  (check-equivalence '(integer (9)) '(integer 10))
  nil)

(deftest subtypep.integer.24
  (check-equivalence '(integer * (11)) '(integer * 10))
  nil)

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

;;; Tests of EQL types

(deftest subtypep.eql.1
  (let ((s1 (copy-seq "abc"))
	(s2 (copy-seq "abc")))
    (let ((t1 `(eql ,s1))
	  (t2 `(eql ,s2)))
      (cond
       ((subtypep t1 t2) "T1 is subtype of T2")
       ((subtypep t2 t1) "T2 is subtype of T1")
       (t (check-disjointness t1 t2)))))
  nil)
    
(deftest subtypep.eql.2
  (let ((s1 (copy-seq '(a b c)))
	(s2 (copy-seq '(a b c))))
    (let ((t1 `(eql ,s1))
	  (t2 `(eql ,s2)))
      (cond
       ((subtypep t1 t2) "T1 is subtype of T2")
       ((subtypep t2 t1) "T2 is subtype of T1")
       (t (check-disjointness t1 t2)))))
  nil)



    
