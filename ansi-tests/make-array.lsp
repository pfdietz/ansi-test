;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Sep 20 06:47:37 2002
;;;; Contains: Tests for MAKE-ARRAY

(in-package :cl-test)

(defun make-array-check-upgrading (type)
  (subtypep* type (array-element-type (make-array 0 :element-type type))))

(defun make-array-with-checks (dimensions
			       &rest options
			       &key
			       (element-type t element-type-p)
			       (initial-contents nil intial-contents-p)
			       (initial-element nil initial-element-p)
			       (adjustable nil)
			       (fill-pointer nil)
			       (displaced-to nil)
			       (displaced-index-offset 0 dio-p)
			       &aux
			       (dimensions-list (if (listp dimensions)
						    dimensions
						  (list dimensions))))
  "Call MAKE-ARRAY and do sanity tests on the output."
  (let ((a (apply #'make-array dimensions options)))
    (cond
     ((not (typep a 'array)) :fail-not-array)
     ((and (eq t element-type)
	   (not adjustable)
	   (not fill-pointer)
	   (not displaced-to)
	   (not (typep a 'simple-array)))
      :fail-not-simple-array)
     ((and (eq (length dimensions-list) 1) 
	   (cond
	    ((not (typep a 'vector))
	     :fail-not-vector)
	    ((and (subtypep 'bit element-type)
		  (subtypep element-type 'bit)
		  (or (not (bit-vector-p a))
		      (not (typep a 'bit-vector))))
	     :fail-not-bit-vector)
	    ((and (not adjustable)
		  (not fill-pointer)
		  (not displaced-to)
		  (cond
		   ((and (eq t element-type)
			 (or (not (simple-vector-p a))
			     (not (typep a 'simple-vector))))
		    :fail-not-simple-vector)
		   ((and (subtypep 'bit element-type)
			 (subtypep element-type 'bit)
			 (or (not (simple-bit-vector-p a))
			     (not (typep a 'simple-bit-vector))))
		    :fail-not-simple-bit-vector) ))) )))
     ((not (equal (array-dimensions a) dimensions-list))
      :fail-array-dimensions)
     ((not (equal (array-rank a) (length dimensions-list)))
      :fail-array-rank)
     ((multiple-value-bind (sub good)
	  (subtypep element-type (array-element-type a))
	(and good
	     (not sub)
	     :failed-array-element-type)))
     ((and adjustable
	   (not (adjustable-array-p a))
	   :fail-adjustable))
     ((and fill-pointer
	   (not (array-has-fill-pointer-p a))
	   :fail-has-fill-pointer))
     ((and (integerp fill-pointer)
	   (not (eql fill-pointer (fill-pointer a)))
	   :fail-fill-pointer-1))
     ((and (eq fill-pointer t)
	   (not (eql (first dimensions-list) (fill-pointer a)))
	   :fail-fill-pointer-2))
     (t a))))

(deftest make-array.1
  (let ((a (make-array-with-checks 10)))
    (and (symbolp a) a))
  nil)

(deftest make-array.1a
  (let ((a (make-array-with-checks '(10))))
    (and (symbolp a) a))
  nil)

(deftest make-array.2
  (make-array-with-checks 3 :initial-element 'z)
  #(z z z))

(deftest make-array.2a
  (make-array-with-checks 3 :initial-contents '(a b c))
  #(a b c))

(deftest make-array.2b
  (make-array-with-checks 3 :initial-contents #(a b c))
  #(a b c))

(deftest make-array.2c
  (make-array-with-checks 3 :initial-contents "abc")
  #(#\a #\b #\c))

(deftest make-array.2d
  (make-array-with-checks 3 :initial-contents #*010)
  #(0 1 0))

(deftest make-array.3
  (let ((a (make-array-with-checks 5 :element-type 'bit)))
    (and (symbolp a) a))
  nil)

(deftest make-array.4
  (make-array-with-checks 5 :element-type 'bit :initial-element 1)
  #*11111)

(deftest make-array.4a
  (make-array-with-checks 5 :element-type 'bit :initial-contents '(1 0 0 1 0))
  #*10010)

(deftest make-array.4b
  (make-array-with-checks 5 :element-type 'bit :initial-contents #(1 0 0 1 0))
  #*10010)

(deftest make-array.4c
  (make-array-with-checks 5 :element-type 'bit :initial-contents #*10010)
  #*10010)

(deftest make-array.5
  (let ((a (make-array-with-checks 4 :element-type 'character)))
    (and (symbolp a) a))
  nil)

(deftest make-array.5a
  (let ((a (make-array-with-checks '(4) :element-type 'character)))
    (and (symbolp a) a))
  nil)

(deftest make-array.6
  (make-array-with-checks 4 :element-type 'character
			  :initial-element #\x)
  "xxxx")

(deftest make-array.6a
  (make-array-with-checks 4 :element-type 'character
			  :initial-contents '(#\a #\b #\c #\d))
  "abcd")

(deftest make-array.6b
  (make-array-with-checks 4 :element-type 'character
			  :initial-contents "abcd")
  "abcd")

(deftest make-array.7
  (make-array-with-checks 5 :element-type 'symbol
			  :initial-element 'a)
  #(a a a a a))

(deftest make-array.7a
  (make-array-with-checks 5 :element-type 'symbol
			  :initial-contents '(a b c d e))
  #(a b c d e))

(deftest make-array.7b
  (make-array-with-checks '(5) :element-type 'symbol
			  :initial-contents '(a b c d e))
  #(a b c d e))

(deftest make-array.8
  (let ((a (make-array-with-checks 8 :element-type '(integer 0 (256)))))
    (and (symbolp a) a))
  nil)

(deftest make-array.8a
  (make-array-with-checks 8 :element-type '(integer 0 (256))
			  :initial-element 9)
  #(9 9 9 9 9 9 9 9))

(deftest make-array.8b
  (make-array-with-checks '(8) :element-type '(integer 0 (256))
			  :initial-contents '(4 3 2 1 9 8 7 6))
  #(4 3 2 1 9 8 7 6))

;;; Zero dimensional arrays

(deftest make-array.9
  (let ((a (make-array-with-checks nil)))
    (and (symbolp a) a))
  nil)

(deftest make-array.10
  (make-array-with-checks nil :initial-element 1)
  #0a1)

(deftest make-array.11
  (make-array-with-checks nil :initial-contents 2)
  #0a2)

(deftest make-array.12
  (make-array-with-checks nil :element-type 'bit :initial-contents 1)
  #0a1)

(deftest make-array.13
  (make-array-with-checks nil :element-type t :initial-contents 'a)
  #0aa)

;;; Higher dimensional arrays

(deftest make-array.14
  (let ((a (make-array-with-checks '(2 3))))
    (and (symbolp a) a))
  nil)

(deftest make-array.15
  (make-array-with-checks '(2 3) :initial-element 'x)
  #2a((x x x) (x x x)))

(deftest make-array.16
  (make-array-with-checks '(0 0))
  #2a())

(deftest make-array.17
  (make-array-with-checks '(2 3) :initial-contents '((a b c) (d e f)))
  #2a((a b c) (d e f)))

(deftest make-array.18
  (make-array-with-checks '(2 3) :initial-contents '(#(a b c) #(d e f)))
  #2a((a b c) (d e f)))

;;; Adjustable arrays

(deftest make-array.adjustable.1
  (let ((a (make-array-with-checks '(10) :adjustable t)))
    (and (symbolp a) a))
  nil)

(deftest make-array.adjustable.2
 (make-array-with-checks '(4) :adjustable t
			 :initial-element 6)
 #(6 6 6 6))

(deftest make-array.adjustable.3
  (make-array-with-checks nil :adjustable t :initial-element 7)
  #0a7)

(deftest make-array.adjustable.4
  (make-array-with-checks '(2 3) :adjustable t :initial-element 7)
  #2a((7 7 7) (7 7 7)))



