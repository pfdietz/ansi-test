;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Oct 12 19:41:14 2002
;;;; Contains: Tests on SUBSEQ

(in-package :cl-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; subseq, on lists

(deftest subseq-list-1
  (subseq '(a b c d e) 0 0)
  nil)

(deftest subseq-list-2
  (subseq '(a b c) 0)
  (a b c))

(deftest subseq-list-3
  (subseq '(a b c) 1)
  (b c))

(defun subseq-list-4-body ()
  (block done
    (let ((x (loop for i from 0 to 19 collect i)))
      (loop
       for i from 0 to 20 do
       (loop
	for j from i to 20 do
	(let ((y (subseq x i j)))
	  (loop
	   for e in y and k from i to (1- j) do
	   (unless (eql e k) (return-from done nil)))))))
    t))

(deftest subseq-list-4
  (subseq-list-4-body)
  t)

(defun subseq-list-5-body ()
  (block done
    (let ((x (loop for i from 0 to 29 collect i)))
      (loop
       for i from 0 to 30 do
       (unless (equal (subseq x i)
		      (loop for j from i to 29 collect j))
	 (return-from done nil))))
    t))

(deftest subseq-list-5
  (subseq-list-5-body)
  t)

(defun subseq-list-6-body ()
  (let* ((x (make-list 100))
	 (z (loop for e on x collect e))
	 (y (subseq x 0)))
    (loop
     for e on x
     and f on y
     and g in z do
     (when (or (not (eqt g e))
	       (not (eql (car e) (car f)))
	       (car e)
	       (eqt e f))
       (return nil))
     finally (return t))))

(deftest subseq-list-6    ;; check that no structure is shared
  (subseq-list-6-body)
  t)

(deftest subseq-list-7
  (let ((x (loop for i from 0 to 9 collect i)))
    (setf (subseq x 0 3) (list 'a 'b 'c))
    x)
  (a b c 3 4 5 6 7 8 9))

(deftest subseq-list-8
  (let* ((x '(a b c d e))
	 (y (copy-seq x)))
    (setf (subseq y 0) '(f g h))
    (list x y))
  ((a b c d e) (f g h d e)))

(deftest subseq-list-9
  (let* ((x '(a b c d e))
	 (y (copy-seq x)))
    (setf (subseq y 1 3) '(1 2 3 4 5))
    (list x y))
  ((a b c d e) (a 1 2 d e)))

(deftest subseq-list-10
  (let* ((x '(a b c d e))
	 (y (copy-seq x)))
    (setf (subseq y 5) '(1 2 3 4 5))
    (list x y))
  ((a b c d e) (a b c d e)))

(deftest subseq-list-11
  (let* ((x '(a b c d e))
	 (y (copy-seq x)))
    (setf (subseq y 2 5) '(1))
    (list x y))
  ((a b c d e) (a b 1 d e)))

(deftest subseq-list-12
  (let* ((x '(a b c d e))
	 (y (copy-seq x)))
    (setf (subseq y 0 0) '(1 2))
    (list x y))
  ((a b c d e) (a b c d e)))

;; subseq on vectors

(defun subseq-vector-1-body ()
  (block nil
  (let* ((x (make-sequence 'vector 10 :initial-element 'a))
	 (y (subseq x 4 8)))
    (unless (every #'(lambda (e) (eqt e 'a)) x)
      (return 1))
    (unless (every #'(lambda (e) (eqt e 'a)) y)
      (return 2))
    (unless (eql (length x) 10) (return 3))
    (unless (eql (length y) 4)  (return 4))
    (loop for i from 0 to 9 do (setf (aref x i) 'b))
    (unless (every #'(lambda (e) (eqt e 'a)) y)
      (return 5))
    (loop for i from 0 to 3 do (setf (aref y i) 'c))
    (or
     (not (not (every #'(lambda (e) (eqt e 'b)) x)))
     6))))

(deftest subseq-vector-1
  (subseq-vector-1-body)
  t)

(defun subseq-vector-2-body ()
  (block nil
  (let* ((x (make-sequence '(vector fixnum) 10 :initial-element 1))
	 (y (subseq x 4 8)))
    (unless (every #'(lambda (e) (eql e 1)) x)
      (return 1))
    (unless (every #'(lambda (e) (eql e 1)) y)
      (return 2))
    (unless (eql (length x) 10) (return 3))
    (unless (eql (length y) 4)  (return 4))
    (loop for i from 0 to 9 do (setf (aref x i) 2))
    (unless (every #'(lambda (e) (eql e 1)) y)
      (return 5))
    (loop for i from 0 to 3 do (setf (aref y i) 3))
    (or
     (not (not (every #'(lambda (e) (eql e 2)) x)))
     6))))

(deftest subseq-vector-2
    (subseq-vector-2-body)
  t) 

(defun subseq-vector-3-body ()
  (block nil
  (let* ((x (make-sequence '(vector single-float) 10 :initial-element 1.0))
	 (y (subseq x 4 8)))
    (unless (every #'(lambda (e) (= e 1.0)) x)
      (return 1))
    (unless (every #'(lambda (e) (= e 1.0)) y)
      (return 2))
    (unless (eql (length x) 10) (return 3))
    (unless (eql (length y) 4)  (return 4))
    (loop for i from 0 to 9 do (setf (aref x i) 2.0))
    (unless (every #'(lambda (e) (= e 1.0)) y)
      (return 5))
    (loop for i from 0 to 3 do (setf (aref y i) 3.0))
    (or
     (not (not (every #'(lambda (e) (= e 2.0)) x)))
     6))))

(deftest subseq-vector-3
    (subseq-vector-3-body)
  t) 

(defun subseq-vector-4-body ()
  (block nil
  (let* ((x (make-sequence '(vector double-float) 10 :initial-element 1.0d0))
	 (y (subseq x 4 8)))
    (unless (every #'(lambda (e) (= e 1.0)) x)
      (return 1))
    (unless (every #'(lambda (e) (= e 1.0)) y)
      (return 2))
    (unless (eql (length x) 10) (return 3))
    (unless (eql (length y) 4)  (return 4))
    (loop for i from 0 to 9 do (setf (aref x i) 2.0d0))
    (unless (every #'(lambda (e) (= e 1.0)) y)
      (return 5))
    (loop for i from 0 to 3 do (setf (aref y i) 3.0d0))
    (or
     (not (not (every #'(lambda (e) (= e 2.0)) x)))
     6))))

(deftest subseq-vector-4
    (subseq-vector-4-body)
  t) 

(defun subseq-vector-5-body ()
  (block nil
  (let* ((x (make-sequence '(vector short-float) 10 :initial-element 1.0s0))
	 (y (subseq x 4 8)))
    (unless (every #'(lambda (e) (= e 1.0)) x)
      (return 1))
    (unless (every #'(lambda (e) (= e 1.0)) y)
      (return 2))
    (unless (eql (length x) 10) (return 3))
    (unless (eql (length y) 4)  (return 4))
    (loop for i from 0 to 9 do (setf (aref x i) 2.0s0))
    (unless (every #'(lambda (e) (= e 1.0)) y)
      (return 5))
    (loop for i from 0 to 3 do (setf (aref y i) 3.0s0))
    (or
     (not (not (every #'(lambda (e) (= e 2.0)) x)))
     6))))

(deftest subseq-vector-5
    (subseq-vector-5-body)
  t) 

(defun subseq-vector-6-body ()
  (block nil
  (let* ((x (make-sequence '(vector long-float) 10 :initial-element 1.0l0))
	 (y (subseq x 4 8)))
    (unless (every #'(lambda (e) (= e 1.0)) x)
      (return 1))
    (unless (every #'(lambda (e) (= e 1.0)) y)
      (return 2))
    (unless (eql (length x) 10) (return 3))
    (unless (eql (length y) 4)  (return 4))
    (loop for i from 0 to 9 do (setf (aref x i) 2.0l0))
    (unless (every #'(lambda (e) (= e 1.0)) y)
      (return 5))
    (loop for i from 0 to 3 do (setf (aref y i) 3.0l0))
    (or
     (not (not (every #'(lambda (e) (= e 2.0)) x)))
     6))))

(deftest subseq-vector-6
    (subseq-vector-6-body)
  t)

;;; slipped this in here for the moment
(deftest copy-seq-vector-1
    (let* ((x (make-array '(10) :initial-contents '(a b c d e f g h i j)))
	   (y (copy-seq x)))
      (equal-array x y))
  t)

(deftest subseq-vector-7
    (let* ((x (make-array '(10) :initial-contents '(a b c d e f g h i j)))
	   (y (subseq x 2 8)))
      (equal-array y (make-array '(6) :initial-contents '(c d e f g h))))
  t)

(deftest subseq-vector-8
    (let* ((x (make-array '(200) :initial-element 107
			  :element-type 'fixnum))
	   (y (subseq x 17 95)))
      (and (eql (length y) (- 95 17))
	   (equal-array y
			(make-array (list (- 95 17))
				    :initial-element 107
				    :element-type 'fixnum))))
  t)

(deftest subseq-vector-9
    (let* ((x (make-array '(1000) :initial-element 17.6e-1
			  :element-type 'single-float))
	   (lo 164)
	   (hi 873)
	   (y (subseq x lo hi)))
      (and (eql (length y) (- hi lo))
	   (equal-array y
			(make-array (list (- hi lo))
				    :initial-element 17.6e-1
				    :element-type 'single-float))))
  t)

(deftest subseq-vector-10
    (let* ((x (make-array '(2000) :initial-element 3.1415927d4
			  :element-type 'double-float))
	   (lo 731)
	   (hi 1942)
	   (y (subseq x lo hi)))
      (and (eql (length y) (- hi lo))
	   (equal-array y
			(make-array (list (- hi lo))
				    :initial-element  3.1415927d4
				    :element-type 'double-float))))
  t)

