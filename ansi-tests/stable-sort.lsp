;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Aug 28 21:00:44 2002
;;;; Contains: Tests for STABLE-SORT

(in-package :cl-test)

(deftest stable-sort-list.1
  (let ((a (list 1 4 2 5 3)))
    (stable-sort a #'<))
  (1 2 3 4 5))

(deftest stable-sort-list.2
  (let ((a (list 1 4 2 5 3)))
    (stable-sort a #'< :key #'-))
  (5 4 3 2 1))

(deftest stable-sort-list.3
  (let ((a (list 1 4 2 5 3)))
    (stable-sort a #'(lambda (x y) nil))
    (stable-sort a #'<))
  (1 2 3 4 5))

(deftest stable-sort-list.4
  (let ((a (copy-seq '((1 a) (2 a) (1 b) (2 b) (1 c) (2 c)))))
    (stable-sort a #'(lambda (x y) (< (car x) (car y)))))
  ((1 a) (1 b) (1 c) (2 a) (2 b) (2 c)))

(deftest stable-sort-list.5
  (let ((a (reverse (copy-seq '((1 a) (2 a) (1 b) (2 b) (1 c) (2 c))))))
    (stable-sort a #'(lambda (x y) (< (car x) (car y)))))
  ((1 c) (1 b) (1 a) (2 c) (2 b) (2 a)))

(deftest stable-sort-vector.1
  (let ((a (copy-seq #(1 4 2 5 3))))
    (stable-sort a #'<))
  #(1 2 3 4 5))

(deftest stable-sort-vector.2
  (let ((a (copy-seq #(1 4 2 5 3))))
    (stable-sort a #'< :key #'-))
  #(5 4 3 2 1))

(deftest stable-sort-vector.3
  (let ((a (copy-seq #(1 4 2 5 3))))
    (stable-sort a #'(lambda (x y) nil))
    (stable-sort a #'<))
  #(1 2 3 4 5))

(deftest stable-sort-vector.4
  (let ((a (make-array 10 :initial-contents '(10 40 20 50 30 15 45 25 55 35)
		       :fill-pointer 5)))
    (stable-sort a #'<))
  #(10 20 30 40 50))

(deftest stable-sort-bit-vector.1
  (let ((a (copy-seq #*10011101)))
    (stable-sort a #'<))
  #*00011111)

(deftest stable-sort-bit-vector.2
  (let ((a (copy-seq #*10011101)))
    (values (stable-sort a #'< :key #'-) a))
  #*11111000
  #*11111000)

(deftest stable-sort-bit-vector.3
  (let ((a (make-array 10 :initial-contents '(1 0 0 1 1 1 1 0 1 1)
		       :element-type 'bit
		       :fill-pointer 5)))
    (stable-sort a #'<))
  #*00111)

(deftest stable-sort-string.1
  (let ((a (copy-seq "10011101")))
    (values (stable-sort a #'char<) a))
  "00011111"
  "00011111")

(deftest stable-sort-string.2
  (let ((a (copy-seq "10011101")))
    (values (stable-sort a #'char<
			 :key #'(lambda (c) (if (eql c #\0) #\1 #\0))) a))
  "11111000"
  "11111000")

(deftest stable-sort-string.3
  (let ((a (make-array 10 :initial-contents "1001111011"
		       :element-type 'character
		       :fill-pointer 5)))
    (stable-sort a #'char<))
  "00111")

;;; Order of evaluation tests

(deftest stable-sort.order.1
  (let ((i 0) x y)
    (values
     (stable-sort (progn (setf x (incf i)) (list 1 7 3 2))
		  (progn (setf y (incf i)) #'<))
     i x y))
  (1 2 3 7) 2 1 2)

(deftest stable-sort.order.2
  (let ((i 0) x y z)
    (values
     (stable-sort (progn (setf x (incf i)) (list 1 7 3 2))
		  (progn (setf y (incf i)) #'<)
		  :key (progn (setf z (incf i)) #'-))
     i x y z))
  (7 3 2 1) 3 1 2 3)


;;; Error cases

(deftest stable-sort.error.1
  (classify-error (stable-sort))
  program-error)

(deftest stable-sort.error.2
  (classify-error (stable-sort nil))
  program-error)

(deftest stable-sort.error.3
  (classify-error (stable-sort nil #'< :key))
  program-error)

(deftest stable-sort.error.4
  (classify-error (stable-sort nil #'< 'bad t))
  program-error)

(deftest stable-sort.error.5
  (classify-error (stable-sort nil #'< 'bad t :allow-other-keys nil))
  program-error)

(deftest stable-sort.error.6
  (classify-error (stable-sort nil #'< 1 2))
  program-error)

(deftest stable-sort.error.7
  (classify-error (stable-sort (list 1 2 3 4) #'identity))
  program-error)

(deftest stable-sort.error.8
  (classify-error (stable-sort (list 1 2 3 4) #'< :key #'cons))
  program-error)

(deftest stable-sort.error.9
  (classify-error (stable-sort (list 1 2 3 4) #'< :key #'car))
  type-error)

(deftest stable-sort.error.10
  (classify-error (stable-sort (list 1 2 3 4) #'elt))
  type-error)
