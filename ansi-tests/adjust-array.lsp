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
    (assert (if (adjustable-array-p a1)
		(eq a1 a2)
	      (equal (array-dimensions a1) '(5))))
    (assert (equal (multiple-value-list (array-displacement a2))
		   (list a0 0)))
    a2)
  #(1 2 3 4))

(deftest adjust-array.13
  (let* ((a0 (make-array 7 :initial-contents '(1 2 3 4 5 6 7)))
	 (a1 (make-array 5 :initial-contents '(a b c d e)))
	 (a2 (adjust-array a1 4 :displaced-to a0
			   :displaced-index-offset 2)))
    (assert (if (adjustable-array-p a1)
		(eq a1 a2)
	      (equal (array-dimensions a1) '(5))))
    (assert (equal (multiple-value-list (array-displacement a2))
		   (list a0 2)))
    a2)
  #(3 4 5 6))

(deftest adjust-array.14
  (let* ((a0 (make-array 7 :initial-contents '(1 2 3 4 5 6 7)))
	 (a1 (make-array 5 :displaced-to a0 :displaced-index-offset 1))
	 (a2 (adjust-array a1 4 :displaced-to a0)))
    (assert (if (adjustable-array-p a1)
		(eq a1 a2)
	      (equal (array-dimensions a1) '(5))))
    (assert (equal (multiple-value-list (array-displacement a2))
		   (list a0 0)))
    a2)
  #(1 2 3 4))

(deftest adjust-array.15
  (let* ((a0 (make-array 7 :initial-contents '(1 2 3 4 5 6 7)))
	 (a1 (make-array 5 :displaced-to a0 :displaced-index-offset 1))
	 (a2 (make-array 4 :displaced-to a1 :displaced-index-offset 1))
	 (a3 (adjust-array a2 4 :displaced-to a1)))
    a3)
  #(2 3 4 5))

(deftest adjust-array.16
  (let* ((a0 (make-array 7 :initial-contents '(1 2 3 4 5 6 7)))
	 (a1 (make-array 5 :displaced-to a0 :displaced-index-offset 1))
	 (a2 (adjust-array a1 5 :displaced-to a0)))
    a2)
  #(1 2 3 4 5))

(deftest adjust-array.17
  (let* ((a0 (make-array nil :initial-element 'x))
	 (a1 (adjust-array a0 nil)))
    (values a0 a1))
  #0ax #0ax)

(deftest adjust-array.18
  (let* ((a0 (make-array nil :initial-element 'x))
	 (a1 (adjust-array a0 nil :initial-contents 'y)))
    (assert (if (adjustable-array-p a0)
		(eq a0 a1)
	      (eq (aref a0) 'x)))
    a1)
  #0ay)

(deftest adjust-array.19
  (let* ((a0 (make-array nil :initial-element 'x))
	 (a1 (adjust-array a0 nil :initial-element 'y)))
    (values a0 a1))
  #0ax #0ax)

(deftest adjust-array.20
  (let* ((a0 (make-array nil :initial-element 'x))
	 (a1 (make-array nil :displaced-to a0))
	 (a2 (adjust-array a1 nil)))
    a2)
  #0ax)

;; 2-d arrays

(deftest adjust-array.21
  (let* ((a1 (make-array '(4 5) :initial-contents '((1 2 3 4 5)
						    (3 4 5 6 7)
						    (5 6 7 8 9)
						    (7 8 9 1 2))))
	 (a2 (adjust-array a1 '(2 3))))
    (assert (if (adjustable-array-p a1)
		(eq a1 a2)
	      (equal (array-dimensions a1) '(2 3))))
    a2)
  #2a((1 2 3)(3 4 5)))

(deftest adjust-array.22
  (let* ((a1 (make-array '(4 5) :initial-contents '((1 2 3 4 5)
						    (3 4 5 6 7)
						    (5 6 7 8 9)
						    (7 8 9 1 2))))
	 (a2 (adjust-array a1 '(6 8) :initial-element 0)))
    (assert (if (adjustable-array-p a1)
		(eq a1 a2)
	      (equal (array-dimensions a1) '(6 8))))
    a2)
  #2a((1 2 3 4 5 0 0 0)
      (3 4 5 6 7 0 0 0)
      (5 6 7 8 9 0 0 0)
      (7 8 9 1 2 0 0 0)
      (0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0)))

(deftest adjust-array.23
  (let* ((a1 (make-array '(4 5) :initial-contents '((#\1 #\2 #\3 #\4 #\5)
						    (#\3 #\4 #\5 #\6 #\7)
						    (#\5 #\6 #\7 #\8 #\9)
						    (#\7 #\8 #\9 #\1 #\2))
			 :element-type 'character))
	 (a2 (adjust-array a1 '(2 3) :element-type 'character)))
    (assert (if (adjustable-array-p a1)
		(eq a1 a2)
	      (equal (array-dimensions a1) '(2 3))))
    (assert (not (typep 0 (array-element-type a2))))
    a2)
  #2a((#\1 #\2 #\3)(#\3 #\4 #\5)))

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

(deftest adjust-array.adjustable.12
  (let* ((a0 (make-array 7 :initial-contents '(x a b c d e y)))
	 (a1 (make-array 5 :displaced-to a0 :displaced-index-offset 1
			 :adjustable t))
	 (a2 (adjust-array a1 4 :displaced-to a0)))
    (assert (eq a1 a2))
    a2)
  #(x a b c))

(deftest adjust-array.adjustable.13
  (let* ((a0 (make-array 7 :initial-contents '(x a b c d e y)))
	 (a1 (make-array 5 :displaced-to a0 :displaced-index-offset 1
			 :adjustable t))
	 (a2 (make-array 4 :displaced-to a1 :displaced-index-offset 1)))
    (assert (eq a1 (adjust-array a1 5 :displaced-to a0
				 :displaced-index-offset 2)))
    a2)
  #(c d e y))


;;; FIXME.  Tests for:
;;;  strings/character arrays
;;;  bit vectors/arrays
;;;  specialized integer arrays
;;;  float arrays

;;; Error cases

(deftest adjust-array.error.1
  (signals-error (adjust-array) program-error)
  t)

(deftest adjust-array.error.2
  (signals-error (adjust-array (make-array 10 :initial-element nil))
		 program-error)
  t)

(deftest adjust-array.error.3
  (signals-error (adjust-array (make-array 10 :initial-element nil)
			       8 :bad t)
		 program-error)
  t)

(deftest adjust-array.error.4
  (signals-error (adjust-array (make-array 10 :initial-element nil)
			       8 :initial-element)
		 program-error)
  t)

(deftest adjust-array.error.5
  (signals-error (adjust-array (make-array 10 :initial-element nil)
			       8
			       :allow-other-keys nil
			       :allow-other-keys t
			       :bad t)
		 program-error)
  t)

(deftest adjust-array.error.6
  (signals-error
   (let ((a (make-array 5 :initial-element 'x)))
     (adjust-array a :fill-pointer 4))
   error)
  t)
