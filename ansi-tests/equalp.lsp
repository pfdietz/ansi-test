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

(deftest equalp.order.1
  (let ((i 0) x y)
    (values
     (equalp (setf x (incf i)) (setf y (incf i)))
     i x y))
  nil 2 1 2)

(deftest equalp.error.1
  (classify-error (equalp))
  program-error)

(deftest equalp.error.2
  (classify-error (equalp nil))
  program-error)

(deftest equalp.error.3
  (classify-error (equalp nil nil nil))
  program-error)
