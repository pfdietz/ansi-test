;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Aug 23 22:08:57 2002
;;;; Contains: Tests for POSITION-IF

(in-package :cl-test)

(deftest position-if-list.1
  (position-if #'evenp '(1 3 1 4 3 2 1 8 9))
  3)

(deftest position-if-list.2
  (position-if 'evenp '(1 3 1 4 3 2 1 8 9))
  3)

(deftest position-if-list.3
  (position-if #'evenp '(1 3 1 4 3 2 1 8 9) :start 4)
  5)

(deftest position-if-list.4
  (position-if #'evenp '(1 3 1 4 3 2 1 8 9) :from-end t)
  7)

(deftest position-if-list.5
  (position-if #'evenp '(1 3 1 4 3 2 1 8 9) :from-end nil)
  3)

(deftest position-if-list.6
  (position-if #'evenp '(1 3 1 4 3 2 1 8 9) :start 4
	       :from-end t)
  7)

(deftest position-if-list.7
  (position-if #'evenp '(1 3 1 4 3 2 1 8 9) :end nil)
  3)

(deftest position-if-list.8
  (position-if #'evenp '(1 3 1 4 3 2 1 8 9) :end 3)
  nil)

(deftest position-if-list.9
  (loop for i from 0 to 8
	collect
	(loop for j from (1+ i) to 9
	      collect
	      (position-if #'evenp '(1 3 1 4 3 2 1 8 9) :start i :end j)))
  ((nil nil nil 3 3 3 3 3 3)
   (nil nil 3 3 3 3 3 3)
   (nil 3 3 3 3 3 3)
   (3 3 3 3 3 3)
   (nil 5 5 5 5)
   (5 5 5 5)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-list.10
  (loop for i from 0 to 8
	collect
	(loop for j from (1+ i) to 9
	      collect
	      (position-if #'evenp '(1 3 1 4 3 2 1 8 9) :start i :end j
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

(deftest position-if-list.11
  (loop for i from 0 to 8
	collect
	(loop for j from (1+ i) to 9
	      collect
	      (position-if #'oddp '(1 3 1 4 3 2 1 8 9) :start i :end j
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

(deftest position-if-list.12
  (loop for i from 0 to 8
	collect
	(loop for j from (1+ i) to 9
	      collect
	      (position-if #'oddp '(1 3 1 4 3 2 1 8 9) :start i :end j
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

(deftest position-if-vector.1
  (position-if #'evenp #(1 3 1 4 3 2 1 8 9))
  3)

(deftest position-if-vector.2
  (position-if 'evenp #(1 3 1 4 3 2 1 8 9))
  3)

(deftest position-if-vector.3
  (position-if #'evenp #(1 3 1 4 3 2 1 8 9) :start 4)
  5)

(deftest position-if-vector.4
  (position-if #'evenp #(1 3 1 4 3 2 1 8 9) :from-end t)
  7)

(deftest position-if-vector.5
  (position-if #'evenp #(1 3 1 4 3 2 1 8 9) :from-end nil)
  3)

(deftest position-if-vector.6
  (position-if #'evenp #(1 3 1 4 3 2 1 8 9) :start 4
	       :from-end t)
  7)

(deftest position-if-vector.7
  (position-if #'evenp #(1 3 1 4 3 2 1 8 9) :end nil)
  3)

(deftest position-if-vector.8
  (position-if #'evenp #(1 3 1 4 3 2 1 8 9) :end 3)
  nil)

(deftest position-if-vector.9
  (loop for i from 0 to 8
	collect
	(loop for j from (1+ i) to 9
	      collect
	      (position-if #'evenp #(1 3 1 4 3 2 1 8 9) :start i :end j)))
  ((nil nil nil 3 3 3 3 3 3)
   (nil nil 3 3 3 3 3 3)
   (nil 3 3 3 3 3 3)
   (3 3 3 3 3 3)
   (nil 5 5 5 5)
   (5 5 5 5)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-vector.10
  (loop for i from 0 to 8
	collect
	(loop for j from (1+ i) to 9
	      collect
	      (position-if #'evenp #(1 3 1 4 3 2 1 8 9) :start i :end j
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

(deftest position-if-vector.11
  (loop for i from 0 to 8
	collect
	(loop for j from (1+ i) to 9
	      collect
	      (position-if #'oddp #(1 3 1 4 3 2 1 8 9) :start i :end j
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

(deftest position-if-vector.12
  (loop for i from 0 to 8
	collect
	(loop for j from (1+ i) to 9
	      collect
	      (position-if #'oddp #(1 3 1 4 3 2 1 8 9) :start i :end j
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

;;; Bit vector tests

(deftest position-if-bit-vector.1
  (position-if #'evenp #*111010101)
  3)

(deftest position-if-bit-vector.2
  (position-if 'evenp #*111010101)
  3)

(deftest position-if-bit-vector.3
  (position-if #'evenp #*111010101 :start 4)
  5)

(deftest position-if-bit-vector.4
  (position-if #'evenp #*111010101 :from-end t)
  7)

(deftest position-if-bit-vector.5
  (position-if #'evenp #*111010101 :from-end nil)
  3)

(deftest position-if-bit-vector.6
  (position-if #'evenp #*111010101 :start 4
	       :from-end t)
  7)

(deftest position-if-bit-vector.7
  (position-if #'evenp #*111010101 :end nil)
  3)

(deftest position-if-bit-vector.8
  (position-if #'evenp #*111010101 :end 3)
  nil)

(deftest position-if-bit-vector.9
  (loop for i from 0 to 8
	collect
	(loop for j from (1+ i) to 9
	      collect
	      (position-if #'evenp #*111010101 :start i :end j)))
  ((nil nil nil 3 3 3 3 3 3)
   (nil nil 3 3 3 3 3 3)
   (nil 3 3 3 3 3 3)
   (3 3 3 3 3 3)
   (nil 5 5 5 5)
   (5 5 5 5)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-bit-vector.10
  (loop for i from 0 to 8
	collect
	(loop for j from (1+ i) to 9
	      collect
	      (position-if #'evenp #*111010101 :start i :end j
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

(deftest position-if-bit-vector.11
  (loop for i from 0 to 8
	collect
	(loop for j from (1+ i) to 9
	      collect
	      (position-if #'oddp #*111010101 :start i :end j
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

(deftest position-if-bit-vector.12
  (loop for i from 0 to 8
	collect
	(loop for j from (1+ i) to 9
	      collect
	      (position-if #'oddp #*111010101 :start i :end j
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

;;; string tests

(deftest position-if-string.1
  (position-if #'evendigitp "131432189")
  3)

(deftest position-if-string.2
  (position-if 'evendigitp "131432189")
  3)

(deftest position-if-string.3
  (position-if #'evendigitp "131432189" :start 4)
  5)

(deftest position-if-string.4
  (position-if #'evendigitp "131432189" :from-end t)
  7)

(deftest position-if-string.5
  (position-if #'evendigitp "131432189" :from-end nil)
  3)

(deftest position-if-string.6
  (position-if #'evendigitp "131432189" :start 4
	       :from-end t)
  7)

(deftest position-if-string.7
  (position-if #'evendigitp "131432189" :end nil)
  3)

(deftest position-if-string.8
  (position-if #'evendigitp "131432189" :end 3)
  nil)

(deftest position-if-string.9
  (loop for i from 0 to 8
	collect
	(loop for j from (1+ i) to 9
	      collect
	      (position-if #'evendigitp "131432189" :start i :end j)))
  ((nil nil nil 3 3 3 3 3 3)
   (nil nil 3 3 3 3 3 3)
   (nil 3 3 3 3 3 3)
   (3 3 3 3 3 3)
   (nil 5 5 5 5)
   (5 5 5 5)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-string.10
  (loop for i from 0 to 8
	collect
	(loop for j from (1+ i) to 9
	      collect
	      (position-if #'evendigitp "131432189" :start i :end j
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

(deftest position-if-string.11
  (loop for i from 0 to 8
	collect
	(loop for j from (1+ i) to 9
	      collect
	      (position-if #'odddigitp "131432189" :start i :end j
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

(deftest position-if-string.12
  (loop for i from 0 to 8
	collect
	(loop for j from (1+ i) to 9
	      collect
	      (position-if #'odddigitp "131432189" :start i :end j
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

;;; Error tests

(deftest position-if-error.1
  (classify-error (position-if #'identity 'b))
  type-error)

(deftest position-if-error.2
  (classify-error (position-if #'identity 10))
  type-error)

(deftest position-if-error.3
  (classify-error (position-if 'null 1.4))
  type-error)

(deftest position-if-error.4
  (classify-error (position-if 'null '(a b c . d)))
  type-error)
