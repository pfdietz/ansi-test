;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 28 07:29:48 1998
;;;; Contains: Testing of CL Features related to "CONS", part 1

(in-package :cl-test)

(declaim (optimize (safety 3)))

;;
;; Test the subtype relationships between null, list, cons and atom
;;
(deftest subtypep-null-list
  (subtypep* 'null 'list)
  t t)

(deftest subtypep-cons-list
  (subtypep* 'cons 'list)
  t t)

(deftest subtypep-null-cons
  (subtypep* 'null 'cons)
  nil t)

(deftest subtypep-cons-null
  (subtypep* 'cons 'null)
  nil t)

(deftest subtypep-null-atom
  (subtypep* 'null 'atom)
  t t)

(deftest subtypep-cons-atom
  (subtypep* 'cons 'atom)
  nil t)

(deftest subtypep-atom-cons
  (subtypep* 'atom 'cons)
  nil t)

(deftest subtypep-atom-list
  (subtypep* 'atom 'list)
  nil t)

(deftest subtypep-list-atom
  (subtypep* 'list 'atom)
  nil t)

;;
;; Check that the elements of *universe* in type null
;; are those for which the null predice is true.
;;
(deftest null-null-universe
  (check-type-predicate 'null 'null)
  0)

(defvar *cons-fns*
  (list 'cons 'consp 'atom 'rplaca 'rplacd
	'car 'cdr 'caar 'cadr 'cdar 'cddr
	'caaar 'caadr 'cadar 'caddr
	'cdaar 'cdadr 'cddar 'cdddr
	'caaaar 'caaadr 'caadar 'caaddr
	'cadaar 'cadadr 'caddar 'cadddr
	'cdaaar 'cdaadr 'cdadar 'cdaddr
	'cddaar 'cddadr 'cdddar 'cddddr
	'copy-tree 'sublis 'nsublis
	'subst 'subst-if 'subst-if-not
	'nsubst 'nsubst-if 'nsubst-if-not
	'tree-equal
	'copy-list
	'list
	'list*
	'list-length
	'listp
	'make-list
	'first 'second 'third 'fourth
	'fifth 'sixth 'seventh 'eighth 'ninth 'tenth
	'nth
	'endp
	'null
	'nconc
	'append
	'revappend 'nreconc
	'butlast 'nbutlast
	'last 'ldiff 'tailp
	'nthcdr 'rest
	'member 'member-if 'member-if-not
	'mapc 'mapcar 'mapcan 'mapl 'maplist 'mapcon
	'acons
	'assoc 'assoc-if 'assoc-if-not
	'copy-alist
	'pairlis
	'rassoc 'rassoc-if 'rassoc-if-not
	'get-properties
	'getf
	'intersection
	'nintersection
	'adjoin
	'set-difference 'nset-difference
	'set-exclusive-or 'nset-exclusive-or
	'subsetp
	'union 'nunion
	))

;; All the cons functions have a function binding

(deftest function-bound-cons-fns
  (loop
   for x in *cons-fns* count
   (when (or (not (fboundp x))
	     (not (functionp (symbol-function x))))
     (format t "~%~S not bound to a function" x)
     t))
  0)

;; All the cons-related macros have a macro binding
(deftest macro-bound-cons-macros
  (notnot-mv (every #'macro-function
		    (list 'push 'pop 'pushnew 'remf)))
  t)

;; None of the cons-related functions have macro bindings
(deftest no-cons-fns-are-macros
  (some #'macro-function *cons-fns*)
  nil)

;; Various easy tests of cons
(deftest cons-of-symbols
  (cons 'a 'b)
  (a . b))

(deftest cons-with-nil
  (cons 'a nil)
  (a))

;; successive calls to cons produces results that are equal, but not eq
(deftest cons-eq-equal
  (let ((x (cons 'a 'b))
	(y (cons 'a 'b)))
    (and (not (eqt x y))
	 (equalt x y)))
  t)

;; list can be expressed as a bunch of conses (with nil)
(deftest cons-equal-list
  (equalt (cons 'a (cons 'b (cons 'c nil)))
	  (list 'a 'b 'c))
  t)

;;; Order of evaluation of cons arguments
(deftest cons.order.1
  (let ((i 0)) (values (cons (incf i) (incf i)) i))
  (1 . 2) 2)

;; Lists satisfy consp
(deftest consp-list
  (notnot-mv (consp '(a)))
  t)

;; cons satisfies consp
(deftest consp-cons
  (notnot-mv (consp (cons nil nil)))
  t)

;; nil is not a consp
(deftest consp-nil
  (consp nil)
  nil)

;; The empty list is not a cons
(deftest consp-empty-list
  (consp (list))
  nil)

;; A single element list is a cons
(deftest consp-single-element-list
  (notnot-mv (consp (list 'a)))
  t)

;; For everything in *universe*, it is either an atom, or satisfies
;; consp, but not both
(deftest consp-xor-atom-universe
  (notnot-mv
   (every #'(lambda (x) (or (and (consp x) (not (atom x)))
			    (and (not (consp x)) (atom x))))
	  *universe*))
  t)

;; Everything in type cons satisfies consp, and vice versa
(deftest consp-cons-universe
  (check-type-predicate 'consp 'cons)
  0)

(deftest consp.order.1
  (let ((i 0))
    (values (consp (incf i)) i))
  nil 1)

(deftest consp.error.1
  (classify-error (consp))
  program-error)

(deftest consp.error.2
  (classify-error (consp 'a 'b))
  program-error)

(deftest atom.order.1
  (let ((i 0))
    (values (atom (progn (incf i) '(a b))) i))
  nil 1)

(deftest atom.error.1
  (classify-error (atom))
  program-error)

(deftest atom.error.2
  (classify-error (atom 'a 'b))
  program-error)

;; Tests of car, cdr and compound forms
(deftest cons.23
  (car '(a))
  a)

(deftest cons.24
  (cdr '(a . b))
  b)

(deftest cons.25
  (caar '((a)))
  a)

(deftest cons.26
  (cdar '((a . b)))
  b)

(deftest cons.27
  (cadr '(a b))
  b)

(deftest cons.28
  (cddr '(a b . c))
  c)

(deftest cons.29
  (caaar '(((a))))
  a)

(deftest cons.30
  (cdaar '(((a . b))))
  b)

(deftest cons.31
  (cadar (cons (cons 'a (cons 'b 'c)) 'd))
  b)

(deftest cons.32
  (cddar (cons (cons 'a (cons 'b 'c)) 'd))
  c)

(deftest cons.33
  (caadr (cons 'a (cons (cons 'b 'c) 'd)))
  b)

(deftest cons.34
  (caddr (cons 'a (cons 'b (cons 'c 'd))))
  c)

(deftest cons.36
  (cdadr (cons 'a (cons (cons 'b 'c) 'd)))
  c)

(deftest cons.37
  (cdddr (cons 'a (cons 'b (cons 'c 'd))))
  d)

(defvar *cons-test-4*
  (cons (cons (cons (cons 'a 'b)
		    (cons 'c 'd))
	      (cons (cons 'e 'f)
		    (cons 'g 'h)))
	(cons (cons (cons 'i 'j)
		    (cons 'k 'l))
	      (cons (cons 'm 'n)
		    (cons 'o 'p)))))


(deftest cons.38
  (caaaar *cons-test-4*)
  a)

(deftest cons.39
  (cdaaar *cons-test-4*)
  b)

(deftest cons.40
  (cadaar *cons-test-4*)
  c)

(deftest cons.41
  (cddaar *cons-test-4*)
  d)

(deftest cons.42
  (caadar *cons-test-4*)
  e)

(deftest cons.43
  (cdadar *cons-test-4*)
  f)

(deftest cons.44
  (caddar *cons-test-4*)
  g)

(deftest cons.45
  (cdddar *cons-test-4*)
  h)

;;;

(deftest cons.46
  (caaadr *cons-test-4*)
  i)

(deftest cons.47
  (cdaadr *cons-test-4*)
  j)

(deftest cons.48
  (cadadr *cons-test-4*)
  k)

(deftest cons.49
  (cddadr *cons-test-4*)
  l)

(deftest cons.50
  (caaddr *cons-test-4*)
  m)

(deftest cons.51
  (cdaddr *cons-test-4*)
  n)

(deftest cons.52
  (cadddr *cons-test-4*)
  o)

(deftest cons.53
  (cddddr *cons-test-4*)
  p)

(deftest cons.error.1
  (classify-error (cons))
  program-error)

(deftest cons.error.2
  (classify-error (cons 'a))
  program-error)

(deftest cons.error.3
  (classify-error (cons 'a 'b 'c))
  program-error)

;; Test rplaca, rplacd

(deftest rplaca.1
  (let ((x (cons 'a 'b)))
    (let ((y x))
      (and (eqt (rplaca x 'c) y)
	   (eqt x y)
	   (eqt (car x) 'c)
	   (eqt (cdr x) 'b))))
  t)

(deftest rplaca.order.1
  (let ((x (cons 'a 'b))
	(i 0) a b)
    (values
     (rplaca (progn (setf a (incf i)) x)
	     (progn (setf b (incf i)) 'c))
     i a b))
  (c . b) 2 1 2)

(deftest rplacd.1
  (let ((x (cons 'a 'b)))
    (let ((y x))
      (and (eqt (rplacd x 'd) y)
	   (eqt x y)
	   (eqt (car x) 'a)
	   (eqt (cdr x) 'd))))
  t)

(deftest rplacd.order.1
  (let ((x (cons 'a 'b))
	(i 0) a b)
    (values
     (rplacd (progn (setf a (incf i)) x)
	     (progn (setf b (incf i)) 'c))
     i a b))
  (a . c) 2 1 2)

;; rplaca on a fixnum is a type error
(deftest rplaca.error.1
  (loop for x in *universe*
	thereis (and (not (consp x))
		     (not (eq (catch-type-error (rplaca x 1)) 'type-error))))
  nil)

(deftest rplaca.error.2
  (classify-error (rplaca))
  program-error)

(deftest rplaca.error.3
  (classify-error (rplaca (cons 'a 'b)))
  program-error)

(deftest rplaca.error.4
  (classify-error (rplaca (cons 'a 'b) (cons 'c 'd) 'garbage))
  program-error)

(deftest rplaca.error.5
  (classify-error (rplaca 'a 1))
  type-error)

(deftest rplaca.error.6
  (classify-error (locally (rplaca 'a 1) t))
  type-error)

;; rplacd on a fixnum is a type error
(deftest rplacd.error.1
  (loop for x in *universe*
	thereis (and (not (consp x))
		     (not (eq (catch-type-error (rplacd x 1)) 'type-error))))
  nil)

(deftest rplacd.error.2
  (classify-error (rplacd))
  program-error)

(deftest rplacd.error.3
  (classify-error (rplacd (cons 'a 'b)))
  program-error)

(deftest rplacd.error.4
  (classify-error (rplacd (cons 'a 'b) (cons 'c 'd) 'garbage))
  program-error)

(deftest rplacd.error.5
  (classify-error (rplacd 'a 1))
  type-error)

(deftest rplacd.error.6
  (classify-error (locally (rplacd 'a 1) t))
  type-error)
