;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Oct 12 19:41:14 2002
;;;; Contains: Tests on SUBSEQ

(in-package :cl-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; subseq, on lists

(deftest subseq-list.1
  (subseq '(a b c d e) 0 0)
  nil)

(deftest subseq-list.2
  (subseq '(a b c) 0)
  (a b c))

(deftest subseq-list.3
  (subseq '(a b c) 1)
  (b c))


(deftest subseq-list.4
  (subseq-list.4-body)
  t)

(deftest subseq-list.5
  (subseq-list.5-body)
  t)

(deftest subseq-list.6    ;; check that no structure is shared
  (subseq-list.6-body)
  t)

(deftest subseq-list.7
  (let ((x (loop for i from 0 to 9 collect i)))
    (setf (subseq x 0 3) (list 'a 'b 'c))
    x)
  (a b c 3 4 5 6 7 8 9))

(deftest subseq-list.8
  (let* ((x '(a b c d e))
	 (y (copy-seq x)))
    (setf (subseq y 0) '(f g h))
    (list x y))
  ((a b c d e) (f g h d e)))

(deftest subseq-list.9
  (let* ((x '(a b c d e))
	 (y (copy-seq x)))
    (setf (subseq y 1 3) '(1 2 3 4 5))
    (list x y))
  ((a b c d e) (a 1 2 d e)))

(deftest subseq-list.10
  (let* ((x '(a b c d e))
	 (y (copy-seq x)))
    (setf (subseq y 5) '(1 2 3 4 5))
    (list x y))
  ((a b c d e) (a b c d e)))

(deftest subseq-list.11
  (let* ((x '(a b c d e))
	 (y (copy-seq x)))
    (setf (subseq y 2 5) '(1))
    (list x y))
  ((a b c d e) (a b 1 d e)))

(deftest subseq-list.12
  (let* ((x '(a b c d e))
	 (y (copy-seq x)))
    (setf (subseq y 0 0) '(1 2))
    (list x y))
  ((a b c d e) (a b c d e)))

;; subseq on vectors


(deftest subseq-vector.1
  (subseq-vector.1-body)
  t)


(deftest subseq-vector.2
    (subseq-vector.2-body)
  t) 


(deftest subseq-vector.3
    (subseq-vector.3-body)
  t) 

(deftest subseq-vector.4
    (subseq-vector.4-body)
  t) 

(deftest subseq-vector.5
  (subseq-vector.5-body)
  t) 

(deftest subseq-vector.6
  (subseq-vector.6-body)
  t)

(deftest subseq-vector.7
    (let* ((x (make-array '(10) :initial-contents '(a b c d e f g h i j)))
	   (y (subseq x 2 8)))
      (equal-array y (make-array '(6) :initial-contents '(c d e f g h))))
  t)

(deftest subseq-vector.8
    (let* ((x (make-array '(200) :initial-element 107
			  :element-type 'fixnum))
	   (y (subseq x 17 95)))
      (and (eqlt (length y) (- 95 17))
	   (equal-array y
			(make-array (list (- 95 17))
				    :initial-element 107
				    :element-type 'fixnum))))
  t)

(deftest subseq-vector.9
    (let* ((x (make-array '(1000) :initial-element 17.6e-1
			  :element-type 'single-float))
	   (lo 164)
	   (hi 873)
	   (y (subseq x lo hi)))
      (and (eqlt (length y) (- hi lo))
	   (equal-array y
			(make-array (list (- hi lo))
				    :initial-element 17.6e-1
				    :element-type 'single-float))))
  t)

(deftest subseq-vector.10
    (let* ((x (make-array '(2000) :initial-element 3.1415927d4
			  :element-type 'double-float))
	   (lo 731)
	   (hi 1942)
	   (y (subseq x lo hi)))
      (and (eqlt (length y) (- hi lo))
	   (equal-array y
			(make-array (list (- hi lo))
				    :initial-element  3.1415927d4
				    :element-type 'double-float))))
  t)

;;; subseq on strings

(deftest subseq-string.1
  (subseq-string.1-body)
  t)

(deftest subseq-string.2
  (subseq-string.2-body)
  t)

(deftest subseq-string.3
  (subseq-string.3-body)
  t)

;;; Tests on bit vectors

(deftest subseq-bit-vector.1
  (subseq-bit-vector.1-body)
  t)

(deftest subseq-bit-vector.2
  (subseq-bit-vector.2-body)
  t)

(deftest subseq-bit-vector.3
  (subseq-bit-vector.3-body)
  t)

;;; Order of evaluation

(deftest subseq.order.1
  (let ((i 0) a b c)
    (values
     (subseq
      (progn (setf a (incf i)) "abcdefgh")
      (progn (setf b (incf i)) 1)
      (progn (setf c (incf i)) 4))
     i a b c))
  "bcd" 3 1 2 3)

(deftest subseq.order.2
  (let ((i 0) a b)
    (values
     (subseq
      (progn (setf a (incf i)) "abcdefgh")
      (progn (setf b (incf i)) 1))
     i a b))
  "bcdefgh" 2 1 2)

(deftest subseq.order.3
  (let ((i 0) a b c d
	(s (copy-seq "abcdefgh")))
    (values
     (setf (subseq
	    (progn (setf a (incf i)) s)
	    (progn (setf b (incf i)) 1)
	    (progn (setf c (incf i)) 4))
	   (progn (setf d (incf i)) "xyz"))
     s i a b c d))
  "xyz" "axyzefgh" 4 1 2 3 4)

(deftest subseq.order.4
  (let ((i 0) a b c
	(s (copy-seq "abcd")))
    (values
     (setf (subseq
	    (progn (setf a (incf i)) s)
	    (progn (setf b (incf i)) 1))
	   (progn (setf c (incf i)) "xyz"))
     s i a b c))
  "xyz" "axyz" 3 1 2 3)

;;; Error cases

(deftest subseq.error.1
  (classify-error (subseq))
  program-error)

(deftest subseq.error.2
  (classify-error (subseq nil))
  program-error)

(deftest subseq.error.3
  (classify-error (subseq nil 0 0 0))
  program-error)


