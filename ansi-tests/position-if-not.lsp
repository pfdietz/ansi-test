;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Aug 24 07:10:05 2002
;;;; Contains: Tests for POSITION-IF-NOT-NOT

(in-package :cl-test)

(deftest position-if-not-list.1
  (position-if-not #'oddp '(1 3 1 4 3 2 1 8 9))
  3)

(deftest position-if-not-list.2
  (position-if-not 'oddp '(1 3 1 4 3 2 1 8 9))
  3)

(deftest position-if-not-list.3
  (position-if-not #'oddp '(1 3 1 4 3 2 1 8 9) :start 4)
  5)

(deftest position-if-not-list.4
  (position-if-not #'oddp '(1 3 1 4 3 2 1 8 9) :from-end t)
  7)

(deftest position-if-not-list.5
  (position-if-not #'oddp '(1 3 1 4 3 2 1 8 9) :from-end nil)
  3)

(deftest position-if-not-list.6
  (position-if-not #'oddp '(1 3 1 4 3 2 1 8 9) :start 4
	       :from-end t)
  7)

(deftest position-if-not-list.7
  (position-if-not #'oddp '(1 3 1 4 3 2 1 8 9) :end nil)
  3)

(deftest position-if-not-list.8
  (position-if-not #'oddp '(1 3 1 4 3 2 1 8 9) :end 3)
  nil)

(deftest position-if-not-list.9
  (loop for i from 0 to 8
	collect
	(loop for j from (1+ i) to 9
	      collect
	      (position-if-not #'oddp '(1 3 1 4 3 2 1 8 9) :start i :end j)))
  ((nil nil nil 3 3 3 3 3 3)
   (nil nil 3 3 3 3 3 3)
   (nil 3 3 3 3 3 3)
   (3 3 3 3 3 3)
   (nil 5 5 5 5)
   (5 5 5 5)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-not-list.10
  (loop for i from 0 to 8
	collect
	(loop for j from (1+ i) to 9
	      collect
	      (position-if-not #'oddp '(1 3 1 4 3 2 1 8 9) :start i :end j
			   :from-end t)))
  ((nil nil nil 3 3 5 5 7 7)
   (nil nil 3 3 5 5 7 7)
   (nil 3 3 5 5 7 7)
   (3 3 5 5 7 7)
   (nil 5 5 7 7)
   (5 5 7 7)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-not-list.11
  (loop for i from 0 to 8
	collect
	(loop for j from (1+ i) to 9
	      collect
	      (position-if-not #'evenp '(1 3 1 4 3 2 1 8 9) :start i :end j
			   :key '1+)))
  ((nil nil nil 3 3 3 3 3 3)
   (nil nil 3 3 3 3 3 3)
   (nil 3 3 3 3 3 3)
   (3 3 3 3 3 3)
   (nil 5 5 5 5)
   (5 5 5 5)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-not-list.12
  (loop for i from 0 to 8
	collect
	(loop for j from (1+ i) to 9
	      collect
	      (position-if-not #'evenp '(1 3 1 4 3 2 1 8 9) :start i :end j
			   :key #'1+ :from-end t)))
  ((nil nil nil 3 3 5 5 7 7)
   (nil nil 3 3 5 5 7 7)
   (nil 3 3 5 5 7 7)
   (3 3 5 5 7 7)
   (nil 5 5 7 7)
   (5 5 7 7)
   (nil 7 7)
   (7 7)
   (nil)))

;;; Vector tests

(deftest position-if-not-vector.1
  (position-if-not #'oddp #(1 3 1 4 3 2 1 8 9))
  3)

(deftest position-if-not-vector.2
  (position-if-not 'oddp #(1 3 1 4 3 2 1 8 9))
  3)

(deftest position-if-not-vector.3
  (position-if-not #'oddp #(1 3 1 4 3 2 1 8 9) :start 4)
  5)

(deftest position-if-not-vector.4
  (position-if-not #'oddp #(1 3 1 4 3 2 1 8 9) :from-end t)
  7)

(deftest position-if-not-vector.5
  (position-if-not #'oddp #(1 3 1 4 3 2 1 8 9) :from-end nil)
  3)

(deftest position-if-not-vector.6
  (position-if-not #'oddp #(1 3 1 4 3 2 1 8 9) :start 4
	       :from-end t)
  7)

(deftest position-if-not-vector.7
  (position-if-not #'oddp #(1 3 1 4 3 2 1 8 9) :end nil)
  3)

(deftest position-if-not-vector.8
  (position-if-not #'oddp #(1 3 1 4 3 2 1 8 9) :end 3)
  nil)

(deftest position-if-not-vector.9
  (loop for i from 0 to 8
	collect
	(loop for j from (1+ i) to 9
	      collect
	      (position-if-not #'oddp #(1 3 1 4 3 2 1 8 9) :start i :end j)))
  ((nil nil nil 3 3 3 3 3 3)
   (nil nil 3 3 3 3 3 3)
   (nil 3 3 3 3 3 3)
   (3 3 3 3 3 3)
   (nil 5 5 5 5)
   (5 5 5 5)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-not-vector.10
  (loop for i from 0 to 8
	collect
	(loop for j from (1+ i) to 9
	      collect
	      (position-if-not #'oddp #(1 3 1 4 3 2 1 8 9) :start i :end j
			   :from-end t)))
  ((nil nil nil 3 3 5 5 7 7)
   (nil nil 3 3 5 5 7 7)
   (nil 3 3 5 5 7 7)
   (3 3 5 5 7 7)
   (nil 5 5 7 7)
   (5 5 7 7)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-not-vector.11
  (loop for i from 0 to 8
	collect
	(loop for j from (1+ i) to 9
	      collect
	      (position-if-not #'evenp #(1 3 1 4 3 2 1 8 9) :start i :end j
			   :key '1+)))
  ((nil nil nil 3 3 3 3 3 3)
   (nil nil 3 3 3 3 3 3)
   (nil 3 3 3 3 3 3)
   (3 3 3 3 3 3)
   (nil 5 5 5 5)
   (5 5 5 5)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-not-vector.12
  (loop for i from 0 to 8
	collect
	(loop for j from (1+ i) to 9
	      collect
	      (position-if-not #'evenp #(1 3 1 4 3 2 1 8 9) :start i :end j
			   :key #'1+ :from-end t)))
  ((nil nil nil 3 3 5 5 7 7)
   (nil nil 3 3 5 5 7 7)
   (nil 3 3 5 5 7 7)
   (3 3 5 5 7 7)
   (nil 5 5 7 7)
   (5 5 7 7)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-not-vector.13
  (let ((a (make-array '(10) :initial-contents '(1 2 3 4 5 a b c d e)
		       :fill-pointer 5)))
    (values
     (position-if-not #'numberp a)
     (position-if-not #'symbolp a)
     (position-if-not #'numberp a :from-end t)
     (position-if-not #'symbolp a :from-end t)))
  nil 0 nil 4)

;;; Bit vector tests

(deftest position-if-not-bit-vector.1
  (position-if-not #'oddp #*111010101)
  3)

(deftest position-if-not-bit-vector.2
  (position-if-not 'oddp #*111010101)
  3)

(deftest position-if-not-bit-vector.3
  (position-if-not #'oddp #*111010101 :start 4)
  5)

(deftest position-if-not-bit-vector.4
  (position-if-not #'oddp #*111010101 :from-end t)
  7)

(deftest position-if-not-bit-vector.5
  (position-if-not #'oddp #*111010101 :from-end nil)
  3)

(deftest position-if-not-bit-vector.6
  (position-if-not #'oddp #*111010101 :start 4
	       :from-end t)
  7)

(deftest position-if-not-bit-vector.7
  (position-if-not #'oddp #*111010101 :end nil)
  3)

(deftest position-if-not-bit-vector.8
  (position-if-not #'oddp #*111010101 :end 3)
  nil)

(deftest position-if-not-bit-vector.9
  (loop for i from 0 to 8
	collect
	(loop for j from (1+ i) to 9
	      collect
	      (position-if-not #'oddp #*111010101 :start i :end j)))
  ((nil nil nil 3 3 3 3 3 3)
   (nil nil 3 3 3 3 3 3)
   (nil 3 3 3 3 3 3)
   (3 3 3 3 3 3)
   (nil 5 5 5 5)
   (5 5 5 5)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-not-bit-vector.10
  (loop for i from 0 to 8
	collect
	(loop for j from (1+ i) to 9
	      collect
	      (position-if-not #'oddp #*111010101 :start i :end j
			   :from-end t)))
  ((nil nil nil 3 3 5 5 7 7)
   (nil nil 3 3 5 5 7 7)
   (nil 3 3 5 5 7 7)
   (3 3 5 5 7 7)
   (nil 5 5 7 7)
   (5 5 7 7)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-not-bit-vector.11
  (loop for i from 0 to 8
	collect
	(loop for j from (1+ i) to 9
	      collect
	      (position-if-not #'evenp #*111010101 :start i :end j
			   :key #'1+)))
  ((nil nil nil 3 3 3 3 3 3)
   (nil nil 3 3 3 3 3 3)
   (nil 3 3 3 3 3 3)
   (3 3 3 3 3 3)
   (nil 5 5 5 5)
   (5 5 5 5)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-not-bit-vector.12
  (loop for i from 0 to 8
	collect
	(loop for j from (1+ i) to 9
	      collect
	      (position-if-not #'evenp #*111010101 :start i :end j
			   :key '1+ :from-end t)))
  ((nil nil nil 3 3 5 5 7 7)
   (nil nil 3 3 5 5 7 7)
   (nil 3 3 5 5 7 7)
   (3 3 5 5 7 7)
   (nil 5 5 7 7)
   (5 5 7 7)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-not-bit-vector.13
  (let ((a (make-array '(10) :initial-contents '(1 1 1 1 1 0 0 0 0 0)
		       :fill-pointer 5
		       :element-type 'bit)))
    (values
     (position-if-not #'zerop a)
     (position-if-not (complement #'zerop) a)
     (position-if-not #'zerop a :from-end t)
     (position-if-not (complement #'zerop) a :from-end t)))
  0 nil 4 nil)  

;;; string tests

(deftest position-if-not-string.1
  (position-if-not #'odddigitp "131432189")
  3)

(deftest position-if-not-string.2
  (position-if-not 'odddigitp "131432189")
  3)

(deftest position-if-not-string.3
  (position-if-not #'odddigitp "131432189" :start 4)
  5)

(deftest position-if-not-string.4
  (position-if-not #'odddigitp "131432189" :from-end t)
  7)

(deftest position-if-not-string.5
  (position-if-not #'odddigitp "131432189" :from-end nil)
  3)

(deftest position-if-not-string.6
  (position-if-not #'odddigitp "131432189" :start 4
	       :from-end t)
  7)

(deftest position-if-not-string.7
  (position-if-not #'odddigitp "131432189" :end nil)
  3)

(deftest position-if-not-string.8
  (position-if-not #'odddigitp "131432189" :end 3)
  nil)

(deftest position-if-not-string.9
  (loop for i from 0 to 8
	collect
	(loop for j from (1+ i) to 9
	      collect
	      (position-if-not #'odddigitp "131432189" :start i :end j)))
  ((nil nil nil 3 3 3 3 3 3)
   (nil nil 3 3 3 3 3 3)
   (nil 3 3 3 3 3 3)
   (3 3 3 3 3 3)
   (nil 5 5 5 5)
   (5 5 5 5)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-not-string.10
  (loop for i from 0 to 8
	collect
	(loop for j from (1+ i) to 9
	      collect
	      (position-if-not #'odddigitp "131432189" :start i :end j
			   :from-end t)))
  ((nil nil nil 3 3 5 5 7 7)
   (nil nil 3 3 5 5 7 7)
   (nil 3 3 5 5 7 7)
   (3 3 5 5 7 7)
   (nil 5 5 7 7)
   (5 5 7 7)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-not-string.11
  (loop for i from 0 to 8
	collect
	(loop for j from (1+ i) to 9
	      collect
	      (position-if-not #'evendigitp "131432183" :start i :end j
			   :key #'nextdigit)))
  ((nil nil nil 3 3 3 3 3 3)
   (nil nil 3 3 3 3 3 3)
   (nil 3 3 3 3 3 3)
   (3 3 3 3 3 3)
   (nil 5 5 5 5)
   (5 5 5 5)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-not-string.12
  (loop for i from 0 to 8
	collect
	(loop for j from (1+ i) to 9
	      collect
	      (position-if-not #'evendigitp "131432183" :start i :end j
			   :key 'nextdigit :from-end t)))
  ((nil nil nil 3 3 5 5 7 7)
   (nil nil 3 3 5 5 7 7)
   (nil 3 3 5 5 7 7)
   (3 3 5 5 7 7)
   (nil 5 5 7 7)
   (5 5 7 7)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-not-string.13
  (let ((a (make-array '(10) :initial-contents "55555aaaaa"
		       :fill-pointer 5
		       :element-type 'character)))
    (and (stringp a)
	 (values
	  (position-if-not #'digit-char-p a)
	  (position-if-not (complement #'digit-char-p) a)
	  (position-if-not #'digit-char-p a :from-end t)
	  (position-if-not (complement #'digit-char-p) a :from-end t))))
  nil 0 nil 4)

;;; Error tests

(deftest position-if-not-error.1
  (classify-error (position-if-not #'identity 'b))
  type-error)

(deftest position-if-not-error.2
  (classify-error (position-if-not #'identity 10))
  type-error)

(deftest position-if-not-error.3
  (classify-error (position-if-not 'null 1.4))
  type-error)

(deftest position-if-not-error.4
  (classify-error (position-if-not 'identity '(a b c . d)))
  type-error)

