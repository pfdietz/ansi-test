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

;;; Zero iterations
(deftest do.6
  (do ((i 0 (+ i 10)))
      ((> i -1) i)
    (return 'bad))
  0)

;;; Tests of go tags
(deftest do.7
  (let ((x nil))
    (do ((i 0 (1+ i)))
	((>= i 10) x)
      (go around)
      small
      (push 'a x)
      (go done)
      big
      (push 'b x)
      (go done)
      around
      (if (> i 4) (go big) (go small))
      done))
  (b b b b b a a a a a))

;;; No increment form
(deftest do.8
  (do ((i 0 (1+ i))
       (x nil))
      ((>= i 10) x)
    (push 'a x))
  (a a a a a a a a a a))

;;; No do locals
(deftest do.9
  (let ((i 0))
    (do ()
	((>= i 10) i)
      (incf i)))
  10)


  



