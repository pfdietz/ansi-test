;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Jun  4 21:15:05 2003
;;;; Contains: Tests of TYPE-OF

(in-package :cl-test)

;;;  It turns out I left out an important test of type-of:
;;;  (type-of x) must be a recognizable subtype of every builtin type
;;;  of which x is a member.

(deftest type-of.1
  (loop for x in *universe*
	for tp = (type-of x)
	for failures = (loop for tp2 in *cl-all-type-symbols*
			     when (and (typep x tp2)
				       (not (subtypep tp tp2)))
			     collect tp2)
	when failures collect (list x failures))
  nil)

  
