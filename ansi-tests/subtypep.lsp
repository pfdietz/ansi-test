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

