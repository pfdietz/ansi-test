;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 14 09:48:24 1998
;;;; Contains: Test cases for CL, section 17.1

(in-package :cl-test)
(use-package :rt)

(declaim (optimize (safety 3)))

;; elt on ordinary vectors

(defun make-int-array (n &optional (fn #'make-array))
  (let ((a (funcall fn n)))
    (loop
	for i from 0 to (1- n) do
	  (setf (aref a i) i))
    a))

(deftest elt-v-1
  (handler-case
   (elt (make-array '(0)) 0)
   (type-error () 'type-error)
   (error (c) c))
  type-error)

;; (deftest elt-v-2 (elt (make-array '(1)) 0) nil)  ;; actually undefined
(deftest elt-v-3 (elt (make-array '(5) :initial-contents '(a b c d e)) 0)
  a)
(deftest elt-v-4 (elt (make-array '(5) :initial-contents '(a b c d e)) 2)
  c)
(deftest elt-v-5 (elt (make-array '(5) :initial-contents '(a b c d e)) 4)
  e)

(defun elt-v-6-body ()
  (let ((x (make-int-list 1000)))
    (let ((a (make-array '(1000) :initial-contents x)))
      (loop
	  for i from 0 to 999 do
	    (unless (eql i (elt a i)) (return nil))
	  finally (return t)))))

(deftest elt-v-6
    (elt-v-6-body)
  t)

(deftest elt-v-7
  (let* ((x (make-array '(4) :initial-contents (list 'a 'b 'c 'd)))
	 (y (setf (elt x 0) 'e)))
    (list (elt x 0) (elt x 1) (elt x 2) (elt x 3) y))
  (e b c d e))

(deftest elt-v-8
  (let* ((x (make-array '(4) :initial-contents (list 'a 'b 'c 'd)))
	 (y (setf (elt x 1) 'e)))
    (list (elt x 0) (elt x 1) (elt x 2) (elt x 3) y))
  (a e c d e))

(deftest elt-v-9
  (let* ((x (make-array '(4) :initial-contents (list 'a 'b 'c 'd)))
	 (y (setf (elt x 3) 'e)))
    (list (elt x 0) (elt x 1) (elt x 2) (elt x 3) y))
  (a b c e e))

(deftest elt-v-10
  (handler-case
   (let ((x (make-array '(3) :initial-contents (list 'a 'b 'c))))
     (setf (elt x 4) 'd))
   (type-error () 'type-error)
   (error (c) c))
  type-error)

(deftest elt-v-11
  (handler-case
   (let ((x (make-array '(3) :initial-contents (list 'a 'b 'c))))
     (setf (elt x -100) 'd))
   (type-error () 'type-error)
   (error (c) c))
  type-error)

(deftest elt-v-12
    (let ((x (make-int-array 100000)))
      (elt x 90000))
  90000)

(deftest elt-v-13
  (let ((x (make-int-array 100000)))
    (setf (elt x 80000) 'foo)
    (list (elt x 79999)
	  (elt x 80000)
	  (elt x 80001)))
  (79999 foo 80001))

;;;  Adjustable arrays

(defun make-adj-array (n &key initial-contents)
  (if initial-contents
      (make-array n :adjustable t :initial-contents initial-contents)
    (make-array n :adjustable t)))

(deftest elt-adj-array-1
  (handler-case
   (elt (make-adj-array '(0)) 0)
   (type-error () 'type-error)
   (error (c) c))
  type-error)

;; (deftest elt-adj-array-2 (elt (make-adj-array '(1)) 0) nil) ;; actually undefined 
(deftest elt-adj-array-3 (elt (make-adj-array '(5) :initial-contents '(a b c d e)) 0)
  a)
(deftest elt-adj-array-4 (elt (make-adj-array '(5) :initial-contents '(a b c d e)) 2)
  c)
(deftest elt-adj-array-5 (elt (make-adj-array '(5) :initial-contents '(a b c d e)) 4)
  e)

(defun elt-adj-array-6-body ()
  (let ((x (make-int-list 1000)))
    (let ((a (make-adj-array '(1000) :initial-contents x)))
      (loop
	  for i from 0 to 999 do
	    (unless (eql i (elt a i)) (return nil))
	  finally (return t)))))

(deftest elt-adj-array-6
    (elt-adj-array-6-body)
  t)

(deftest elt-adj-array-7
  (let* ((x (make-adj-array '(4) :initial-contents (list 'a 'b 'c 'd)))
	 (y (setf (elt x 0) 'e)))
    (list (elt x 0) (elt x 1) (elt x 2) (elt x 3) y))
  (e b c d e))

(deftest elt-adj-array-8
  (let* ((x (make-adj-array '(4) :initial-contents (list 'a 'b 'c 'd)))
	 (y (setf (elt x 1) 'e)))
    (list (elt x 0) (elt x 1) (elt x 2) (elt x 3) y))
  (a e c d e))

(deftest elt-adj-array-9
  (let* ((x (make-adj-array '(4) :initial-contents (list 'a 'b 'c 'd)))
	 (y (setf (elt x 3) 'e)))
    (list (elt x 0) (elt x 1) (elt x 2) (elt x 3) y))
  (a b c e e))

(deftest elt-adj-array-10
  (handler-case
   (let ((x (make-adj-array '(3) :initial-contents (list 'a 'b 'c))))
     (setf (elt x 4) 'd))
   (type-error () 'type-error)
   (error (c) c))
  type-error)

(deftest elt-adj-array-11
  (handler-case
   (let ((x (make-adj-array '(3) :initial-contents (list 'a 'b 'c))))
     (setf (elt x -100) 'd))
   (type-error () 'type-error)
   (error (c) c))
  type-error)

(deftest elt-adj-array-12
    (let ((x (make-int-array 100000 #'make-adj-array)))
      (elt x 90000))
  90000)

(deftest elt-adj-array-13
    (let ((x (make-int-array 100000 #'make-adj-array)))
    (setf (elt x 80000) 'foo)
    (list (elt x 79999)
	  (elt x 80000)
	  (elt x 80001)))
  (79999 foo 80001))

;; displaced arrays

(defvar *displaced* nil)
(setf *displaced* (make-int-array 100000))

(defun make-displaced-array (n displacement)
  (make-array n :displaced-to *displaced*
	      :displaced-index-offset displacement))

(deftest elt-displaced-array-1 
  (handler-case
   (elt (make-displaced-array '(0) 100) 0)
   (type-error () 'type-error)
   (error (c) c))
  type-error)

(deftest elt-displaced-array-2
  (elt (make-displaced-array '(1) 100) 0)
  100)

(deftest elt-displaced-array-3
  (elt (make-displaced-array '(5) 100) 4)
  104)

#|
(deftest elt-displaced-array-4
  (handler-case
   (make-displaced-array '(100) 100000)
   (type-error () 'type-error)
   (error (c) c))
  type-error)
|#

#|
(deftest elt-displaced-array-5
  (handler-case
   (make-displaced-array '(100) (- 100000 50))
   (type-error () 'type-error)
   (error (c) c))
  type-error)
|#

;; subseq vectors

(defun subseq-vector-1-body ()
  (block nil
  (let* ((x (make-sequence 'vector 10 :initial-element 'a))
	 (y (subseq x 4 8)))
    (unless (every #'(lambda (e) (eq e 'a)) x)
      (return 1))
    (unless (every #'(lambda (e) (eq e 'a)) y)
      (return 2))
    (unless (eql (length x) 10) (return 3))
    (unless (eql (length y) 4)  (return 4))
    (loop for i from 0 to 9 do (setf (aref x i) 'b))
    (unless (every #'(lambda (e) (eq e 'a)) y)
      (return 5))
    (loop for i from 0 to 3 do (setf (aref y i) 'c))
    (or
     (not (not (every #'(lambda (e) (eq e 'b)) x)))
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

;; array reversal

(defun equal-array (a1 a2)
  (and (typep a1 'array)
       (typep a2 'array)
       (= (array-rank a1) (array-rank a2))
       (let ((ad (array-dimensions a1)))
	 (and (equal ad (array-dimensions a2))
	      (let ((as (array-total-size a1)))
		(and (= as (array-total-size a2))
		     (loop
			 for i from 0 to (1- as) do
			   (unless (equal (row-major-aref a1 i)
					  (row-major-aref a2 i))
			     (return nil))
			   finally (return t))))))))

;; array length

(deftest array-length-1
    (length (make-array '(20)))
  20)

(deftest array-length-2
    (length (make-array '(100001)))
  100001)

(deftest array-length-3
    (length (make-array '(0)))
  0)

(deftest array-length-4
    (let ((x (make-array '(100) :fill-pointer 10)))
      (length x))
  10)

(deftest array-length-5
    (let ((x (make-array '(100) :fill-pointer 10)))
      (setf (fill-pointer x) 20)
      (length x))
  20)

;;; Fill on arrays

(deftest array-fill-1
  (let* ((a (make-array '(5) :initial-contents '(a b c d e)))
	 (b (fill a 'x)))
    (values (eq a b)
	    (map 'list #'identity a)))
  t (x x x x x))

(deftest array-fill-2
  (let* ((a (make-array '(5) :initial-contents '(a b c d e)))
	 (b (fill a 'x :start 2)))
    (values (eq a b)
	    (map 'list #'identity a)))
  t (a b x x x))

(deftest array-fill-3
  (let* ((a (make-array '(5) :initial-contents '(a b c d e)))
	 (b (fill a 'x :end 2)))
    (values (eq a b)
	    (map 'list #'identity a)))
  t (x x c d e))

(deftest array-fill-4
  (let* ((a (make-array '(5) :initial-contents '(a b c d e)))
	 (b (fill a 'x :start 1 :end 3)))
    (values (eq a b)
	    (map 'list #'identity a)))
  t (a x x x e))

(deftest array-fill-5
  (let* ((a (make-array '(5) :initial-contents '(a b c d e)))
	 (b (fill a 'x :start 1 :end nil)))
    (values (eq a b)
	    (map 'list #'identity a)))
  t (a x x x x))

(deftest array-fill-6
  (let* ((a (make-array '(5) :initial-contents '(a b c d e)))
	 (b (fill a 'x :end nil)))
    (values (eq a b)
	    (map 'list #'identity a)))
  t (x x x x x))

(deftest array-fill-7
  (let* ((a (make-array '(5))))
    (handler-case (fill a 'x :start -1)
		  (type-error (c) 'type-error)
		  (error (c) c)))
  type-error)

(deftest array-fill-8
  (let* ((a (make-array '(5))))
    (handler-case (fill a 'x :start 'a)
		  (type-error (c) 'type-error)
		  (error (c) c)))
  type-error)

(deftest array-fill-9
  (let* ((a (make-array '(5))))
    (handler-case (fill a 'x :end -1)
		  (type-error (c) 'type-error)
		  (error (c) c)))
  type-error)

(deftest array-fill-10
  (let* ((a (make-array '(5))))
    (handler-case (fill a 'x :end 'a)
		  (type-error (c) 'type-error)
		  (error (c) c)))
  type-error)

;;; fill on arrays of fixnums

(deftest array-fixnum-fill-1
  (let* ((a (make-array '(5) :element-type 'fixnum :initial-contents '(1 2 3 4 5)))
	 (b (fill a 6)))
    (values (eq a b)
	    (map 'list #'identity a)))
  t (6 6 6 6 6))

(deftest array-fixnum-fill-2
  (let* ((a (make-array '(5) :element-type 'fixnum :initial-contents '(1 2 3 4 5)))
	 (b (fill a 6 :start 2)))
    (values (eq a b)
	    (map 'list #'identity a)))
  t (1 2 6 6 6))

(deftest array-fixnum-fill-3
  (let* ((a (make-array '(5) :element-type 'fixnum :initial-contents '(1 2 3 4 5)))
	 (b (fill a 7 :end 2)))
    (values (eq a b)
	    (map 'list #'identity a)))
  t (7 7 3 4 5))

(deftest array-fixnum-fill-4
  (let* ((a (make-array '(5) :element-type 'fixnum :initial-contents '(1 2 3 4 5)))
	 (b (fill a 8 :start 1 :end 3)))
    (values (eq a b)
	    (map 'list #'identity a)))
  t (1 8 8 4 5))

(deftest array-fixnum-fill-5
  (let* ((a (make-array '(5) :element-type 'fixnum :initial-contents '(1 2 3 4 5)))
	 (b (fill a 0 :start 1 :end nil)))
    (values (eq a b)
	    (map 'list #'identity a)))
  t (1 0 0 0 0))

(deftest array-fixnum-fill-6
  (let* ((a (make-array '(5) :element-type 'fixnum :initial-contents '(1 2 3 4 5)))
	 (b (fill a -1 :end nil)))
    (values (eq a b)
	    (map 'list #'identity a)))
  t (-1 -1 -1 -1 -1))

(deftest array-fixnum-fill-7
  (let* ((a (make-array '(5) :element-type 'fixnum)))
    (handler-case (fill a 10 :start -1)
		  (type-error (c) 'type-error)
		  (error (c) c)))
  type-error)

(deftest array-fixnum-fill-8
  (let* ((a (make-array '(5) :element-type 'fixnum)))
    (handler-case (fill a 100 :start 'a)
		  (type-error (c) 'type-error)
		  (error (c) c)))
  type-error)

(deftest array-fixnum-fill-9
  (let* ((a (make-array '(5) :element-type 'fixnum)))
    (handler-case (fill a -5 :end -1)
		  (type-error (c) 'type-error)
		  (error (c) c)))
  type-error)

(deftest array-fixnum-fill-10
  (let* ((a (make-array '(5) :element-type 'fixnum)))
    (handler-case (fill a 17 :end 'a)
		  (type-error (c) 'type-error)
		  (error (c) c)))
  type-error)

;;; fill on arrays of unsigned eight bit bytes

(defun array-unsigned-byte-fill-test-fn (byte-size &rest fill-args)
  (let* ((a (make-array '(5) :element-type (list 'unsigned-byte byte-size)
			:initial-contents '(1 2 3 4 5)))
	 (b (apply #'fill a fill-args)))
    (values (eq a b)
	    (map 'list #'identity a))))

(deftest array-unsigned-byte8-fill-1
  (array-unsigned-byte-fill-test-fn 8 6)
  t (6 6 6 6 6))

(deftest array-unsigned-byte8-fill-2
  (array-unsigned-byte-fill-test-fn 8 6 :start 2)
  t (1 2 6 6 6))

(deftest array-unsigned-byte8-fill-3
  (array-unsigned-byte-fill-test-fn 8 7 :end 2)
  t (7 7 3 4 5))

(deftest array-unsigned-byte8-fill-4
  (array-unsigned-byte-fill-test-fn 8 8 :start 1 :end 3)
  t (1 8 8 4 5))

(deftest array-unsigned-byte8-fill-5
  (array-unsigned-byte-fill-test-fn 8 9 :start 1 :end nil)
  t (1 9 9 9 9))
Z
(deftest array-unsigned-byte8-fill-6
  (array-unsigned-byte-fill-test-fn 8 0 :end nil)
  t (0 0 0 0 0))

(deftest array-unsigned-byte8-fill-7
  (handler-case (array-unsigned-byte-fill-test-fn 8 0 :start -1)
		(type-error (c) 'type-error)
		(error (c) c))
  type-error)

(deftest array-unsigned-byte8-fill-8
    (handler-case (array-unsigned-byte-fill-test-fn 8 100 :start 'a)
		  (type-error (c) 'type-error)
		  (error (c) c))
  type-error)

(deftest array-unsigned-byte8-fill-9
  (handler-case (array-unsigned-byte-fill-test-fn 8 19 :end -1)
		(type-error (c) 'type-error)
		(error (c) c))
  type-error)

(deftest array-unsigned-byte8-fill-10
  (handler-case (array-unsigned-byte-fill-test-fn 8 17 :end 'a)
		(type-error (c) 'type-error)
		(error (c) c))
  type-error)

