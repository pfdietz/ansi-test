;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Oct 12 19:38:29 2002
;;;; Contains: Tests of ELT

(in-package :cl-test)

(declaim (optimize (safety 3)))

;; elt on lists

(deftest elt.1
  (classify-error (elt nil 0))
  type-error)

(deftest elt.1a
  (classify-error (elt nil -10))
  type-error)

(deftest elt.1b
  (classify-error (locally (elt nil 0) t))
  type-error)

(deftest elt.2
  (classify-error (elt nil 1000000))
  type-error)

(deftest elt.3 (elt '(a b c d e) 0) a)
(deftest elt.4 (elt '(a b c d e) 2) c)
(deftest elt.5 (elt '(a b c d e) 4) e)
(deftest elt.5a 
  (classify-error (elt '(a b c d e) -4))
  type-error)

(deftest elt.6
  (let ((x (make-int-list 1000)))
    (notnot-mv
     (every
      #'(lambda (i) (eql i (elt x i)))
      x)))
  t)

(deftest elt.7
  (let* ((x (list 'a 'b 'c 'd))
	 (y (setf (elt x 0) 'e)))
    (list x y))
  ((e b c d) e))

(deftest elt.8
  (let* ((x (list 'a 'b 'c 'd))
	 (y (setf (elt x 1) 'e)))
    (list x y))
  ((a e c d) e))

(deftest elt.9
  (let* ((x (list 'a 'b 'c 'd))
	 (y (setf (elt x 3) 'e)))
    (list x y))
  ((a b c e) e))

(deftest elt.10
  (classify-error
   (let ((x (list 'a 'b 'c)))
     (setf (elt x 4) 'd)))
  type-error)

(deftest elt.11
  (let ((x (list 'a 'b 'c 'd 'e)))
    (let ((y (loop for c on x collect c)))
      (setf (elt x 2) 'f)
      (notnot-mv
       (every #'eq
	      y
	      (loop for c on x collect c)))))
  t)

(deftest elt.12
  (let ((x (make-int-list 100000)))
    (elt x 90000))
  90000)

(deftest elt.13
  (let ((x (make-int-list 100000)))
    (setf (elt x 80000) 'foo)
    (list (elt x 79999)
	  (elt x 80000)
	  (elt x 80001)))
  (79999 foo 80001))

(deftest elt.14
  (classify-error
   (let ((x (list 'a 'b 'c)))
     (elt x 10)))
  type-error)

(deftest elt.15
  (classify-error
   (let ((x (list 'a 'b 'c)))
     (elt x 'a)))
  type-error)

(deftest elt.16
  (classify-error
   (let ((x (list 'a 'b 'c)))
     (elt x 10.0)))
  type-error)

(deftest elt.17
  (classify-error
   (let ((x (list 'a 'b 'c)))
     (elt x -1)))
  type-error)

(deftest elt.18
  (classify-error
   (let ((x (list 'a 'b 'c)))
     (elt x -100000000000000000)))
  type-error)

(deftest elt.19
  (classify-error
   (let ((x (list 'a 'b 'c)))
     (elt x #\w)))
  type-error)

(deftest elt.order.1
  (let ((i 0) x y)
    (values
     (elt (progn (setf x (incf i)) '(a b c d e))
	  (progn (setf y (incf i)) 3))
     i x y))
  d 2 1 2)

(deftest elt.order.2
  (let ((i 0) x y z)
    (let ((a (make-array 1 :initial-element (list 'a 'b 'c 'd 'e))))
      (values
       (setf (elt (aref a (progn (setf x (incf i)) 0))
		  (progn (setf y (incf i)) 3))
	     (progn (setf z (incf i)) 'k))
       (aref a 0)
       i x y z)))
  k (a b c k e) 3 1 2 3)

(deftest elt-v.1
  (classify-error
   (elt (make-array '(0)) 0))
  type-error)

;; (deftest elt-v.2 (elt (make-array '(1)) 0) nil)  ;; actually undefined
(deftest elt-v.3
  (elt (make-array '(5) :initial-contents '(a b c d e)) 0)
  a)

(deftest elt-v.4
  (elt (make-array '(5) :initial-contents '(a b c d e)) 2)
  c)

(deftest elt-v.5
  (elt (make-array '(5) :initial-contents '(a b c d e)) 4)
  e)

(deftest elt-v.6
    (elt-v-6-body)
  t)

(deftest elt-v.7
  (let* ((x (make-array '(4) :initial-contents (list 'a 'b 'c 'd)))
	 (y (setf (elt x 0) 'e)))
    (list (elt x 0) (elt x 1) (elt x 2) (elt x 3) y))
  (e b c d e))

(deftest elt-v.8
  (let* ((x (make-array '(4) :initial-contents (list 'a 'b 'c 'd)))
	 (y (setf (elt x 1) 'e)))
    (list (elt x 0) (elt x 1) (elt x 2) (elt x 3) y))
  (a e c d e))

(deftest elt-v.9
  (let* ((x (make-array '(4) :initial-contents (list 'a 'b 'c 'd)))
	 (y (setf (elt x 3) 'e)))
    (list (elt x 0) (elt x 1) (elt x 2) (elt x 3) y))
  (a b c e e))

(deftest elt-v.10
  (classify-error
   (let ((x (make-array '(3) :initial-contents (list 'a 'b 'c))))
     (setf (elt x 4) 'd)))
  type-error)

(deftest elt-v.11
  (classify-error
   (let ((x (make-array '(3) :initial-contents (list 'a 'b 'c))))
     (setf (elt x -100) 'd)))
  type-error)

(deftest elt-v.12
    (let ((x (make-int-array 100000)))
      (elt x 90000))
  90000)

(deftest elt-v.13
  (let ((x (make-int-array 100000)))
    (setf (elt x 80000) 'foo)
    (list (elt x 79999)
	  (elt x 80000)
	  (elt x 80001)))
  (79999 foo 80001))

;;;  Adjustable arrays

(deftest elt-adj-array.1
  (classify-error (elt (make-adj-array '(0)) 0))
  type-error)

;;; (deftest elt-adj-array.2 (elt (make-adj-array '(1)) 0) nil) ;; actually undefined 

(deftest elt-adj-array.3
 (elt (make-adj-array '(5) :initial-contents '(a b c d e)) 0)
  a)

(deftest elt-adj-array.4
 (elt (make-adj-array '(5) :initial-contents '(a b c d e)) 2)
  c)

(deftest elt-adj-array.5
 (elt (make-adj-array '(5) :initial-contents '(a b c d e)) 4)
  e)

(deftest elt-adj-array.6
    (elt-adj-array-6-body)
  t)

(deftest elt-adj-array.7
  (let* ((x (make-adj-array '(4) :initial-contents (list 'a 'b 'c 'd)))
	 (y (setf (elt x 0) 'e)))
    (list (elt x 0) (elt x 1) (elt x 2) (elt x 3) y))
  (e b c d e))

(deftest elt-adj-array.8
  (let* ((x (make-adj-array '(4) :initial-contents (list 'a 'b 'c 'd)))
	 (y (setf (elt x 1) 'e)))
    (list (elt x 0) (elt x 1) (elt x 2) (elt x 3) y))
  (a e c d e))

(deftest elt-adj-array.9
  (let* ((x (make-adj-array '(4) :initial-contents (list 'a 'b 'c 'd)))
	 (y (setf (elt x 3) 'e)))
    (list (elt x 0) (elt x 1) (elt x 2) (elt x 3) y))
  (a b c e e))

(deftest elt-adj-array.10
  (classify-error
   (let ((x (make-adj-array '(3) :initial-contents (list 'a 'b 'c))))
     (setf (elt x 4) 'd)))
  type-error)

(deftest elt-adj-array.11
  (classify-error
   (let ((x (make-adj-array '(3) :initial-contents (list 'a 'b 'c))))
     (setf (elt x -100) 'd)))
  type-error)

(deftest elt-adj-array.12
    (let ((x (make-int-array 100000 #'make-adj-array)))
      (elt x 90000))
  90000)

(deftest elt-adj-array.13
    (let ((x (make-int-array 100000 #'make-adj-array)))
    (setf (elt x 80000) 'foo)
    (list (elt x 79999)
	  (elt x 80000)
	  (elt x 80001)))
  (79999 foo 80001))

;; displaced arrays

(deftest elt-displaced-array.1 
  (classify-error (elt (make-displaced-array '(0) 100) 0))
  type-error)

(deftest elt-displaced-array.2
  (elt (make-displaced-array '(1) 100) 0)
  100)

(deftest elt-displaced-array.3
  (elt (make-displaced-array '(5) 100) 4)
  104)

;;; Arrays with fill points

(deftest elt-fill-pointer.1
  (let ((a (make-array '(5) :initial-contents '(a b c d e)
		       :fill-pointer 3)))
    (values (elt a 0) (elt a 1) (elt a 2)))
  a b c)

(deftest elt-fill-pointer.2
  (let ((a (make-array '(5)
		       :initial-contents '(0 0 1 0 0)
		       :element-type 'bit
		       :fill-pointer 3)))
    (values (elt a 0) (elt a 1) (elt a 2)))
  0 0 1)

(deftest elt-fill-pointer.3
  (classify-error
   (let ((a (make-array '(5)
			:initial-contents '(0 0 1 0 0)
			:fill-pointer 3)))
     (elt a 4)))
  type-error)

(deftest elt-fill-pointer.4
  (classify-error
   (let ((a (make-array '(5)
			:initial-contents '(0 0 1 0 0)
			:element-type 'bit
			:fill-pointer 3)))
     (elt a 4)))
  type-error)

(deftest elt-fill-pointer.5
   (let ((a (make-array '(5)
			:initial-contents '(#\a #\b #\c #\d #\e)
			:element-type 'character
			:fill-pointer 3)))
     (values (elt a 0) (elt a 1) (elt a 2)))
   #\a #\b #\c)

(deftest elt-fill-pointer.6
  (classify-error
   (let ((a (make-array '(5)
			:initial-contents '(#\a #\b #\c #\d #\e)
			:element-type 'character
			:fill-pointer 3)))
     (elt a 4)))
  type-error)

(deftest elt-fill-pointer.7
   (let ((a (make-array '(5)
			:initial-contents '(#\a #\b #\c #\d #\e)
			:element-type 'base-char
			:fill-pointer 3)))
     (values (elt a 0) (elt a 1) (elt a 2)))
   #\a #\b #\c)

(deftest elt-fill-pointer.8
  (classify-error
   (let ((a (make-array '(5)
			:initial-contents '(#\a #\b #\c #\d #\e)
			:element-type 'base-char
			:fill-pointer 3)))
     (elt a 4)))
  type-error)

(deftest elt.error.1
  (classify-error (elt))
  program-error)

(deftest elt.error.2
  (classify-error (elt nil))
  program-error)

(deftest elt.error.3
  (classify-error (elt nil 0 nil))
  program-error)
