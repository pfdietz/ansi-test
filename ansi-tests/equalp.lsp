;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Oct 17 22:14:42 2002
;;;; Contains: Tests for EQUALP

(in-package :cl-test)

(deftest equalp.1
  (loop for c across +base-chars+
	always (loop for d across +base-chars+
		     always (if (char-equal c d) (equalpt c d)
			      (not (equalpt c d)))))
  t)

(deftest equalp.2
  (loop for i from 1 to 100
	always (loop for j from 1 to 100
		     always (if (eqlt i j) (equalpt i j)
			      (not (equalpt i j)))))
  t)

(deftest equalp.3
  (equalpt "abc" "ABC")
  t)

(deftest equalp.4
  (equalpt "abc" "abd")
  nil)

(deftest equalp.5
  :notes (:allow-nil-arrays)
  (equalpt (make-array '(0) :element-type nil) #())
  t)

(deftest equalp.6
  :notes (:allow-nil-arrays)
  (equalpt (make-array '(0) :element-type nil) "")
  t)

(deftest equalp.order.1
  (let ((i 0) x y)
    (values
     (equalp (setf x (incf i)) (setf y (incf i)))
     i x y))
  nil 2 1 2)

;;; Error tests

(deftest equalp.error.1
  (signals-error (equalp) program-error)
  t)

(deftest equalp.error.2
  (signals-error (equalp nil) program-error)
  t)

(deftest equalp.error.3
  (signals-error (equalp nil nil nil) program-error)
  t)
