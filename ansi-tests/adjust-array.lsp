;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Feb 15 07:27:22 2004
;;;; Contains: Tests of ADJUST-ARRAY

(in-package :cl-test)

(deftest adjust-array.1
  (let* ((a1 (make-array 5 :initial-contents '(a b c d e)))
	 (a2 (adjust-array a1 4)))
    (assert (if (adjustable-array-p a1)
		(eq a1 a2)
	      (equal (array-dimensions a1) '(5))))
    (assert (not (array-displacement a2)))
    a2)
  #(a b c d))

(deftest adjust-array.2
  (let* ((a1 (make-array 5 :initial-contents '(a b c d e)))
	 (a2 (adjust-array a1 8 :initial-element 'x)))
    (assert (if (adjustable-array-p a1)
		(eq a1 a2)
	      (equal (array-dimensions a1) '(5))))
    (assert (not (array-displacement a2)))
    a2)
  #(a b c d e x x x))

(deftest adjust-array.3
  (let* ((a1 (make-array 5 :initial-contents '(a b c d e)))
	 (a2 (adjust-array a1 4 :initial-contents '(w x y z))))
    (assert (if (adjustable-array-p a1)
		(eq a1 a2)
	      (equal (array-dimensions a1) '(5))))
    (assert (not (array-displacement a2)))
    a2)
  #(w x y z))

(deftest adjust-array.4
  (let* ((a1 (make-array 5 :initial-contents '(a b c d e)))
	 (a2 (adjust-array a1 8 :initial-contents '(8 7 6 5 4 3 2 1))))
    (assert (if (adjustable-array-p a1)
		(eq a1 a2)
	      (equal (array-dimensions a1) '(5))))
    (assert (not (array-displacement a2)))
    a2)
  #(8 7 6 5 4 3 2 1))

(deftest adjust-array.5
  (let* ((a1 (make-array 5 :initial-contents '(a b c d e)
			 :fill-pointer 3))
	 (a2 (adjust-array a1 4)))
    (assert (if (adjustable-array-p a1)
		(eq a1 a2)
	      (equal (array-dimensions a1) '(5))))
    (assert (not (array-displacement a2)))
    (values
     (array-dimensions a2)
     (fill-pointer a2)
     a2
     (aref a2 3)))
  (4) 3 #(a b c) d)

(deftest adjust-array.6
  (let* ((a1 (make-array 5 :initial-contents '(a b c d e)
			 :fill-pointer 3))
	 (a2 (adjust-array a1 4 :fill-pointer nil)))
    (assert (if (adjustable-array-p a1)
		(eq a1 a2)
	      (equal (array-dimensions a1) '(5))))
    (assert (not (array-displacement a2)))
    (values
     (array-dimensions a2)
     (fill-pointer a2)
     a2
     (aref a2 3)))
  (4) 3 #(a b c) d)

(deftest adjust-array.7
  (let* ((a1 (make-array 5 :initial-contents '(a b c d e)
			 :fill-pointer 3))
	 (a2 (adjust-array a1 4 :fill-pointer t)))
    (assert (if (adjustable-array-p a1)
		(eq a1 a2)
	      (equal (array-dimensions a1) '(5))))
    (assert (not (array-displacement a2)))
    (values
     (array-dimensions a2)
     (fill-pointer a2)
     a2))
  (4) 4 #(a b c d))

(deftest adjust-array.8
  (let* ((a1 (make-array 5 :initial-contents '(a b c d e)
			 :fill-pointer 3))
	 (a2 (adjust-array a1 4 :fill-pointer 2)))
    (assert (if (adjustable-array-p a1)
		(eq a1 a2)
	      (equal (array-dimensions a1) '(5))))
    (assert (not (array-displacement a2)))
    (values
     (array-dimensions a2)
     (fill-pointer a2)
     a2
     (aref a2 2)
     (aref a2 3)))
  (4) 2 #(a b) c d)

(deftest adjust-array.9
  (let* ((a1 (make-array 5 :initial-contents '(a b c d e)
			 :fill-pointer 3))
	 (a2 (adjust-array a1 8 :fill-pointer 5
			   :initial-element 'x)))
    (assert (if (adjustable-array-p a1)
		(eq a1 a2)
	      (equal (array-dimensions a1) '(5))))
    (assert (not (array-displacement a2)))
    (values
     (array-dimensions a2)
     (fill-pointer a2)
     a2
     (aref a2 5)
     (aref a2 6)
     (aref a2 7)))
  (8) 5 #(a b c d e) x x x)

(deftest adjust-array.10
  (let* ((a1 (make-array 5 :initial-contents '(a b c d e)))
	 (a2 (adjust-array a1 4 :displaced-to nil)))
    (assert (if (adjustable-array-p a1)
		(eq a1 a2)
	      (equal (array-dimensions a1) '(5))))
    (assert (not (array-displacement a2)))
    a2)
  #(a b c d))

(deftest adjust-array.11
  (let* ((a0 (make-array 7 :initial-contents '(x a b c d e y)))
	 (a1 (make-array 5 :displaced-to a0 :displaced-index-offset 1))
	 (a2 (adjust-array a1 4)))
    (assert (if (adjustable-array-p a1)
		(eq a1 a2)
	      (equal (array-dimensions a1) '(5))))
    (assert (not (array-displacement a2)))
    a2)
  #(a b c d))

(deftest adjust-array.12
  (let* ((a0 (make-array 7 :initial-contents '(1 2 3 4 5 6 7)))
	 (a1 (make-array 5 :initial-contents '(a b c d e)))
	 (a2 (adjust-array a1 4 :displaced-to a0)))
    (assert (not (eq a1 a2)))
    (assert (equal (multiple-value-list (array-displacement a2))
		   (list a0 0)))
    a2)
  #(1 2 3 4))

(deftest adjust-array.13
  (let* ((a0 (make-array 7 :initial-contents '(1 2 3 4 5 6 7)))
	 (a1 (make-array 5 :initial-contents '(a b c d e)))
	 (a2 (adjust-array a1 4 :displaced-to a0
			   :displaced-index-offset 2)))
    (assert (not (eq a1 a2)))
    (assert (equal (multiple-value-list (array-displacement a2))
		   (list a0 2)))
    a2)
  #(3 4 5 6))



;;; Adjust an adjustable array

(deftest adjust-array.adjustable.1
  (let* ((a1 (make-array 5 :initial-contents '(a b c d e)
			 :adjustable t))
	 (a2 (adjust-array a1 4)))
    (assert (eq a1 a2))
    (assert (not (array-displacement a2)))
    a2)
  #(a b c d))

(deftest adjust-array.adjustable.2
  (let* ((a1 (make-array 5 :initial-contents '(a b c d e)
			 :adjustable t))
	 (a2 (adjust-array a1 8 :initial-element 'x)))
    (assert (eq a1 a2))
    (assert (not (array-displacement a2)))
    a2)
  #(a b c d e x x x))

(deftest adjust-array.adjustable.3
  (let* ((a1 (make-array 5 :initial-contents '(a b c d e)
			 :adjustable t))
	 (a2 (adjust-array a1 4 :initial-contents '(w x y z))))
    (assert (eq a1 a2))
    (assert (not (array-displacement a2)))
    a2)
  #(w x y z))

(deftest adjust-array.adjustable.4
  (let* ((a1 (make-array 5 :initial-contents '(a b c d e)
			 :adjustable t))
	 (a2 (adjust-array a1 8 :initial-contents '(8 7 6 5 4 3 2 1))))
    (assert (eq a1 a2))
    (assert (not (array-displacement a2)))
    a2)
  #(8 7 6 5 4 3 2 1))

(deftest adjust-array.adjustable.5
  (let* ((a1 (make-array 5 :initial-contents '(a b c d e)
			 :fill-pointer 3 :adjustable t))
	 (a2 (adjust-array a1 4)))
    (assert (eq a1 a2))
    (assert (not (array-displacement a2)))
    (values
     (array-dimensions a2)
     (fill-pointer a2)
     a2
     (aref a2 3)))
  (4) 3 #(a b c) d)

(deftest adjust-array.adjustable.6
  (let* ((a1 (make-array 5 :initial-contents '(a b c d e)
			 :fill-pointer 3 :adjustable t))
	 (a2 (adjust-array a1 4 :fill-pointer nil)))
    (assert (eq a1 a2))
    (assert (not (array-displacement a2)))
    (values
     (array-dimensions a2)
     (fill-pointer a2)
     a2
     (aref a2 3)))
  (4) 3 #(a b c) d)

(deftest adjust-array.adjustable.7
  (let* ((a1 (make-array 5 :initial-contents '(a b c d e)
			 :fill-pointer 3 :adjustable t))
	 (a2 (adjust-array a1 4 :fill-pointer t)))
    (assert (eq a1 a2))
    (assert (not (array-displacement a2)))
    (values
     (array-dimensions a2)
     (fill-pointer a2)
     a2))
  (4) 4 #(a b c d))

(deftest adjust-array.adjustable.8
  (let* ((a1 (make-array 5 :initial-contents '(a b c d e)
			 :fill-pointer 3 :adjustable t))
	 (a2 (adjust-array a1 4 :fill-pointer 2)))
    (assert (eq a1 a2))
    (assert (not (array-displacement a2)))
    (values
     (array-dimensions a2)
     (fill-pointer a2)
     a2
     (aref a2 2)
     (aref a2 3)))
  (4) 2 #(a b) c d)

(deftest adjust-array.adjustable.9
  (let* ((a1 (make-array 5 :initial-contents '(a b c d e)
			 :fill-pointer 3 :adjustable t))
	 (a2 (adjust-array a1 8 :fill-pointer 5
			   :initial-element 'x)))
    (assert (eq a1 a2))
    (assert (not (array-displacement a2)))
    (values
     (array-dimensions a2)
     (fill-pointer a2)
     a2
     (aref a2 5)
     (aref a2 6)
     (aref a2 7)))
  (8) 5 #(a b c d e) x x x)

(deftest adjust-array.adjustable.10
  (let* ((a1 (make-array 5 :initial-contents '(a b c d e)
			 :adjustable t))
	 (a2 (adjust-array a1 4 :displaced-to nil)))
    (assert (eq a1 a2))
    (assert (not (array-displacement a2)))
    a2)
  #(a b c d))

(deftest adjust-array.adjustable.11
  (let* ((a0 (make-array 7 :initial-contents '(x a b c d e y)))
	 (a1 (make-array 5 :displaced-to a0 :displaced-index-offset 1
			 :adjustable t))
	 (a2 (adjust-array a1 4)))
    (assert (eq a1 a2))
    (assert (not (array-displacement a2)))
    a2)
  #(a b c d))






