;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jan 25 08:04:35 2003
;;;; Contains: Tests for VECTOR-PUSH-EXTEND

(in-package :cl-test)

(deftest vector-push-extend.1
  (let ((a (make-array '(5) :fill-pointer 2
		       :initial-contents '(a b c d e)))
	(i 0) x y)
    (values
     (fill-pointer a)
     (vector-push-extend (progn (setf x (incf i)) 'x)
			 (progn (setf y (incf i)) a))
     (fill-pointer a)
     a
     i x y))
  2 2 3 #(a b x) 2 1 2)

(deftest vector-push-extend.2
  (let ((a (make-array '(5) :fill-pointer 5
		       :adjustable t
		       :initial-contents '(a b c d e))))
    (values
     (fill-pointer a)
     (vector-push-extend 'x a)
     (fill-pointer a)
     (<= (array-total-size a) 5)
     a))
  5 5 6 nil #(a b c d e x))

(deftest vector-push-extend.3
  (let ((a (make-array '(5) :fill-pointer 2
		       :initial-contents "abcde"
		       :element-type 'base-char)))
    (values
     (fill-pointer a)
     (vector-push-extend #\x a)
     (fill-pointer a)
     a))
  2 2 3 "abx")

(deftest vector-push-extend.4
  (let ((a (make-array '(5) :fill-pointer 5
		       :adjustable t
		       :initial-contents "abcde"
		       :element-type 'base-char))
	(i 0) x y z)
    (values
     (fill-pointer a)
     (vector-push-extend (progn (setf x (incf i)) #\x)
			 (progn (setf y (incf i)) a)
			 (progn (setf z (incf i)) 1))
     (fill-pointer a)
     (<= (array-total-size a) 5)
     a
     i x y z))
  5 5 6 nil "abcdex" 3 1 2 3)

(deftest vector-push-extend.5
  (let ((a (make-array '(5) :fill-pointer 2
		       :initial-contents "abcde"
		       :element-type 'character)))
    (values
     (fill-pointer a)
     (vector-push-extend #\x a)
     (fill-pointer a)
     a))
  2 2 3 "abx")

(deftest vector-push-extend.6
  (let ((a (make-array '(5) :fill-pointer 5
		       :adjustable t
		       :initial-contents "abcde"
		       :element-type 'character)))
    (values
     (fill-pointer a)
     (vector-push-extend #\x a 10)
     (fill-pointer a)
     (<= (array-total-size a) 5)
     a))
  5 5 6 nil "abcdex")

(deftest vector-push-extend.7
  (let ((a (make-array '(5) :fill-pointer 2
		       :initial-contents '(0 1 1 0 0)
		       :element-type 'bit)))
    (values
     (fill-pointer a)
     (vector-push-extend 0 a)
     (fill-pointer a)
     a))
  2 2 3 #*010)

(deftest vector-push-extend.8
  (let ((a (make-array '(5) :fill-pointer 5
		       :adjustable t
		       :initial-contents '(0 0 0 0 0)
		       :element-type 'bit)))
    (values
     (fill-pointer a)
     (vector-push-extend 1 a 100)
     (fill-pointer a)
     (<= (array-total-size a) 5)
     a))
  5 5 6 nil #*000001)

(deftest vector-push-extend.9
  (let ((a (make-array '(5) :fill-pointer 2
		       :initial-contents '(1 2 3 4 5)
		       :element-type 'fixnum)))
    (values
     (fill-pointer a)
     (vector-push-extend 0 a)
     (fill-pointer a)
     a))
  2 2 3 #(1 2 0))

(deftest vector-push-extend.10
  (let ((a (make-array '(5) :fill-pointer 5
		       :adjustable t
		       :initial-contents '(1 2 3 4 5)
		       :element-type 'fixnum)))
    (values
     (fill-pointer a)
     (vector-push-extend 0 a 1)
     (fill-pointer a)
     (<= (array-total-size a) 5)
     a))
  5 5 6 nil #(1 2 3 4 5 0))

(deftest vector-push-extend.11
  (let ((a (make-array '(5) :fill-pointer 2
		       :initial-contents '(1 2 3 4 5)
		       :element-type '(integer 0 (256)))))
    (values
     (fill-pointer a)
     (vector-push-extend 0 a)
     (fill-pointer a)
     a))
  2 2 3 #(1 2 0))

(deftest vector-push-extend.12
  (let ((a (make-array '(5) :fill-pointer 5
		       :adjustable t
		       :initial-contents '(1 2 3 4 5)
		       :element-type '(integer 0 (256)))))
    (values
     (fill-pointer a)
     (vector-push-extend 0 a 1)
     (fill-pointer a)
     (<= (array-total-size a) 5)
     a))
  5 5 6 nil #(1 2 3 4 5 0))

(deftest vector-push-extend.13
  (let ((a (make-array '(5) :fill-pointer 2
		       :initial-contents '(1.0s0 2.0s0 3.0s0 4.0s0 5.0s0)
		       :element-type 'short-float)))
    (values
     (fill-pointer a)
     (vector-push-extend 0.0s0 a)
     (fill-pointer a)
     a))
  2 2 3 #(1.0s0 2.0s0 0.0s0))

(deftest vector-push-extend.14
  (let ((a (make-array '(5) :fill-pointer 5
		       :adjustable t
		       :initial-contents '(1.0s0 2.0s0 3.0s0 4.0s0 5.0s0)
		       :element-type 'short-float)))
    (values
     (fill-pointer a)
     (vector-push-extend 0.0s0 a 1)
     (fill-pointer a)
     (<= (array-total-size a) 5)
     a))
  5 5 6 nil #(1.0s0 2.0s0 3.0s0 4.0s0 5.0s0 0.0s0))

(deftest vector-push-extend.15
  (let ((a (make-array '(5) :fill-pointer 2
		       :initial-contents '(1.0f0 2.0f0 3.0f0 4.0f0 5.0f0)
		       :element-type 'single-float)))
    (values
     (fill-pointer a)
     (vector-push-extend 0.0f0 a)
     (fill-pointer a)
     a))
  2 2 3 #(1.0f0 2.0f0 0.0f0))

(deftest vector-push-extend.16
  (let ((a (make-array '(5) :fill-pointer 5
		       :adjustable t
		       :initial-contents '(1.0f0 2.0f0 3.0f0 4.0f0 5.0f0)
		       :element-type 'single-float)))
    (values
     (fill-pointer a)
     (vector-push-extend 0.0f0 a 1)
     (fill-pointer a)
     (<= (array-total-size a) 5)
     a))
  5 5 6 nil #(1.0f0 2.0f0 3.0f0 4.0f0 5.0f0 0.0f0))


(deftest vector-push-extend.17
  (let ((a (make-array '(5) :fill-pointer 2
		       :initial-contents '(1.0d0 2.0d0 3.0d0 4.0d0 5.0d0)
		       :element-type 'double-float)))
    (values
     (fill-pointer a)
     (vector-push-extend 0.0d0 a)
     (fill-pointer a)
     a))
  2 2 3 #(1.0d0 2.0d0 0.0d0))

(deftest vector-push-extend.18
  (let ((a (make-array '(5) :fill-pointer 5
		       :adjustable t
		       :initial-contents '(1.0d0 2.0d0 3.0d0 4.0d0 5.0d0)
		       :element-type 'double-float)))
    (values
     (fill-pointer a)
     (vector-push-extend 0.0d0 a 1)
     (fill-pointer a)
     (<= (array-total-size a) 5)
     a))
  5 5 6 nil #(1.0d0 2.0d0 3.0d0 4.0d0 5.0d0 0.0d0))

(deftest vector-push-extend.19
  (let ((a (make-array '(5) :fill-pointer 2
		       :initial-contents '(1.0l0 2.0l0 3.0l0 4.0l0 5.0l0)
		       :element-type 'long-float)))
    (values
     (fill-pointer a)
     (vector-push-extend 0.0l0 a)
     (fill-pointer a)
     a))
  2 2 3 #(1.0l0 2.0l0 0.0l0))

(deftest vector-push-extend.20
  (let ((a (make-array '(5) :fill-pointer 5
		       :adjustable t
		       :initial-contents '(1.0l0 2.0l0 3.0l0 4.0l0 5.0l0)
		       :element-type 'long-float)))
    (values
     (fill-pointer a)
     (vector-push-extend 0.0l0 a 1)
     (fill-pointer a)
     (<= (array-total-size a) 5)
     a))
  5 5 6 nil #(1.0l0 2.0l0 3.0l0 4.0l0 5.0l0 0.0l0))



;;; Error tests

(defun vector-push-extend-error-test (seq val)
  (declare (optimize (safety 3)))
  (handler-case
   (eval `(let ((a (copy-seq ,seq)))
	    (declare (optimize (safety 3)))
	    (or (notnot (array-has-fill-pointer-p a))
		(vector-push-extend ',val a 1))))
   (error () t)))

(deftest vector-push-extend.error.1
  (vector-push-extend-error-test #(a b c d) 'x)
  t)

(deftest vector-push-extend.error.2
  (vector-push-extend-error-test #*00000 1)
  t)

(deftest vector-push-extend.error.3
  (vector-push-extend-error-test "abcde" #\x)
  t)

(deftest vector-push-extend.error.4
  (vector-push-extend-error-test #() 'x)
  t)

(deftest vector-push-extend.error.5
  (vector-push-extend-error-test #* 1)
  t)

(deftest vector-push-extend.error.6
  (vector-push-extend-error-test "" #\x)
  t)

(deftest vector-push-extend.error.7
  (vector-push-extend-error-test (make-array '5 :element-type 'base-char
				      :initial-element #\a)
			  #\x)
  t)

(deftest vector-push-extend.error.8
  (vector-push-extend-error-test (make-array '5 :element-type '(integer 0 (256))
				      :initial-element 0)
			  17)
  t)

(deftest vector-push-extend.error.9
  (vector-push-extend-error-test (make-array '5 :element-type 'float
				      :initial-element 1.0)
			  2.0)
  t)

(deftest vector-push-extend.error.10
  (vector-push-extend-error-test (make-array '5 :element-type 'short-float
				      :initial-element 1.0s0)
			  2.0s0)
  t)

(deftest vector-push-extend.error.11
  (vector-push-extend-error-test (make-array '5 :element-type 'long-float
				      :initial-element 1.0l0)
			  2.0l0)
  t)

(deftest vector-push-extend.error.12
  (vector-push-extend-error-test (make-array '5 :element-type 'single-float
				      :initial-element 1.0f0)
			  2.0f0)
  t)

(deftest vector-push-extend.error.13
  (vector-push-extend-error-test (make-array '5 :element-type 'double-float
				      :initial-element 1.0d0)
			  2.0d0)
  t)

(deftest vector-push-extend.error.14
  (classify-error (vector-push-extend))
  program-error)

(deftest vector-push-extend.error.15
  (classify-error (vector-push-extend (vector 1 2 3)))
  program-error)

(deftest vector-push-extend.error.16
  (classify-error (vector-push-extend (vector 1 2 3) 4 1 nil))
  program-error)

(deftest vector-push-extend.error.17
  (handler-case
   (eval
    `(locally
      (declare (optimize (safety 3)))
      (let ((a (make-array '5 :fill-pointer t :adjustable nil
			   :initial-element nil)))
	(or (notnot (adjustable-array-p a))  ; It's actually adjustable, or...
	    (vector-push-extend a 'x)        ; ... this fails
	    ))))
   (error () t))
  t)






