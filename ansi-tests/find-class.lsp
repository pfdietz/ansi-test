;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu May 29 07:15:06 2003
;;;; Contains: Tests of FIND-CLASS

;; find-class is also tested in numerous other places.

(in-package :cl-test)

(deftest find-class.1
  (loop for name in *cl-types-that-are-classes-symbols*
	unless (eq (find-class name) (find-class name))
	collect name)
  nil)

(deftest find-class.2
  (loop for name in *cl-types-that-are-classes-symbols*
	unless (eq (find-class name t) (find-class name))
	collect name)
  nil)

(deftest find-class.3
  (loop for name in *cl-types-that-are-classes-symbols*
	unless (eq (find-class name nil) (find-class name))
	collect name)
  nil)

(deftest find-class.4
  (handler-case
   (progn (eval '(find-class (gensym))) :bad)
   (error () :good))
  :good)

(deftest find-class.5
  (handler-case
   (progn (eval '(find-class (gensym) t)) :bad)
   (error () :good))
  :good)

(deftest find-class.6
  (find-class (gensym) nil)
  nil)

(deftest find-class.7
  (loop for name in *cl-types-that-are-classes-symbols*
	unless (eq (find-class name t nil) (find-class name))
	collect name)
  nil)

(deftest find-class.8
  (loop for name in *cl-types-that-are-classes-symbols*
	unless (eq (find-class name nil nil) (find-class name))
	collect name)
  nil)

(deftest find-class.9
  (macrolet
      ((%m (&environment env)
	   (let ((result
		  (loop for name in *cl-types-that-are-classes-symbols*
			unless (eq (find-class name nil env)
				   (find-class name))
			collect name)))
	     `',result)))
    (%m))
  nil)

(deftest find-class.10
  (macrolet
      ((%m (&environment env)
	   (let ((result
		  (loop for name in *cl-types-that-are-classes-symbols*
			unless (eq (find-class name t env)
				   (find-class name))
			collect name)))
	     `',result)))
    (%m))
  nil)

(deftest find-class.11
  (handler-case
   (progn (eval '(find-class (gensym) 'a nil)) :bad)
   (error () :good))
  :good)

(deftest find-class.12
  (find-class (gensym) nil nil)
  nil)

(deftest find-class.13
  (macrolet
      ((%m (&environment env)
	   `',(find-class (gensym) nil env)))
    (%m))
  nil)

(deftest find-class.14
  (handler-case
   (progn
     (eval '(macrolet
		((%m (&environment env)
		     `',(find-class (gensym) 17 env)))
	      (%m)))
     :bad)
   (error () :good))
  :good)

;;; Need tests of assignment to (FIND-CLASS ...)
;;; Add tests of:
;;;   Setting class to itself
;;;   Changing class to a different class
;;;   Changing to NIL (and that the class object stays around)
;;;   Check that find-class is affected by the assignment, and
;;;    class-name is not.
