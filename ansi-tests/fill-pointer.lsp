;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan 21 22:14:23 2003
;;;; Contains: Tests of FILL-POINTER

(in-package :cl-test)

;;; More tests are in make-array.lsp

(deftest fill-pointer.1
  (fill-pointer (make-array '(10) :fill-pointer 5))
  5)

(deftest fill-pointer.2
  (fill-pointer (make-array '(10) :fill-pointer t))
  10)

(deftest fill-pointer.3
  (let ((a (make-array '(10) :fill-pointer 5
		       :initial-contents '(1 2 3 4 5 6 7 8 9 10))))
    (values
     (fill-pointer a)
     (setf (fill-pointer a) 6)
     a))
  5 6 #(1 2 3 4 5 6))

;;; Error tests

(deftest fill-pointer.error.1
  (classify-error (fill-pointer))
  program-error)

(deftest fill-pointer.error.2
  (classify-error (fill-pointer (make-array '(10) :fill-pointer 4)
				nil))
  program-error)

(deftest fill-pointer.error.3
  (classify-error (fill-pointer (make-array '(10) :fill-pointer nil)))
  type-error)

(deftest fill-pointer.error.4
  (classify-error (fill-pointer #0aNIL))
  type-error)

(deftest fill-pointer.error.5
  (classify-error (fill-pointer #2a((a b c)(d e f))))
  type-error)

(deftest fill-pointer.error.6
  (let (why)
    (loop for e in *mini-universe*
	  when (and (or (not (typep e 'vector))
			(not (array-has-fill-pointer-p e)))
		    (not (eql (setq why (classify-error** `(fill-pointer ',e)))
			      'type-error)))
	  collect (list e why)))
  nil)
