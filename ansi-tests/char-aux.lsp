;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Oct  5 20:15:55 2002
;;;; Contains: Auxiliary functions for character tests

(in-package :cl-test)

(defun is-ordered-by (seq fn)
  (let ((n (length seq)))
    (loop for i from 0 below (1- n)
	  for e = (elt seq i)
	  always
	  (loop for j from (1+ i) below n
		always (funcall fn e (elt seq j))))))

(defun is-antisymmetrically-ordered-by (seq fn)
  (and (is-ordered-by seq fn)
       (is-ordered-by (reverse seq) (complement fn))))

(defun is-case-insensitive (fn)
  (loop for c across +code-chars+
	for c1 = (char-upcase c)
	for c2 = (if (eql c c1) (char-downcase c) c1)
	always
	(loop for d across +code-chars+
	      for d1 = (char-upcase d)
	      for d2 = (if (eql d d1) (char-downcase d) d1)
	      always (equiv (funcall fn c d)
			    (funcall fn c2 d)
			    (funcall fn c d2)
			    (funcall fn c2 d2)))))

(defun equiv (&rest args)
  (declare (dynamic-extent args))
  (cond
   ((null args) t)
   ((car args)
    (every #'identity (cdr args)))
   (t (every #'not (cdr args)))))



