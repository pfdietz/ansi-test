;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Oct 21 22:58:00 2002
;;;; Contains: Tests for iteration forms

(in-package :cl-test)

;;; Confirm that most macros exist

(defparameter *iteration-macros*
  '(do do* dotimes dolist loop))

(deftest iteration-macros
  (remove-if #'macro-function *iteration-macros*)
  nil)

;;; Tests of DO

(deftest do.1
  (do ((i 0 (1+ i)))
      ((>= i 10) i))
  10)

(deftest do.2
  (do ((i 0 (1+ j))
       (j 0 (1+ i)))
      ((>= i 10) (+ i j)))
  20)

(deftest do.3
  (let ((x nil))
    (do ((i 0 (1+ i)))
	((>= i 10) x)
      (push i x)))
  (9 8 7 6 5 4 3 2 1 0))

(deftest do.4
  (let ((x nil))
    (do ((i 0 (1+ i)))
	((>= i 10) x)
      (declare (fixnum i))
      (push i x)))
  (9 8 7 6 5 4 3 2 1 0))

(deftest do.5
  (do ((i 0 (1+ i)))
      (nil)
    (when (> i 10) (return i)))
  11)

