;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Contains: Random type prop tests: structures

(in-package :cl-test)

(defstruct rtpt-1 a b)

(defmethod make-random-element-of-type ((type (eql 'rtpt-1)))
  (make-rtpt-1 :a (make-random-element-of-type t)
	       :b (make-random-element-of-type t)))

(defmethod replicate ((obj rtpt-1))
  (or (gethash obj *replicate-table*)
      (let ((x (make-rtpt-1)))
	(setf (gethash obj *replicate-table*) x)
	(setf (rtpt-1-a x) (replicate (rtpt-1-a obj)))
	(setf (rtpt-1-b x) (replicate (rtpt-1-b obj)))
	x)))

(defmethods make-random-type-containing*
  (1 ((val rtpt-1)) 'rtpt-1))

(def-type-prop-test structure-ref.1 'rtpt-1-a '(rtpt-1) 1)
