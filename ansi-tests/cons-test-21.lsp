;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 28 22:11:27 1998
;;;; Contains: Testing of CL Features related to "CONS", part 21

(in-package :cl-test)
(use-package :rt)
(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nunion

(defun nunion-with-copy (x y &key test test-not)
  (setf x (copy-list x))
  (setf y (copy-list y))
  (cond
   (test (nunion x y :test test))
   (test-not (nunion x y :test-not test-not))
   (t (nunion x y))))

(defun nunion-with-copy-and-key (x y key &key test test-not)
  (setf x (copy-list x))
  (setf y (copy-list y))
  (cond
   (test (nunion x y :key key :test test))
   (test-not (nunion x y :key key :test-not test-not))
   (t (nunion x y :key key))))

(deftest nunion-1
    (nunion nil nil)
  nil)

(deftest nunion-2
    (nunion-with-copy (list 'a) nil)
  (a))

(deftest nunion-3
    (nunion-with-copy (list 'a) (list 'a))
  (a))

(deftest nunion-4
    (nunion-with-copy (list 1) (list 1))
  (1))

(deftest nunion-5
    (let ((x (list 'a 'b)))
      (nunion-with-copy (list x) (list x)))
  ((a b)))

(deftest nunion-6
    (let ((x '(a b c d e f))
	  (y '(z c y a v b)))
      (let ((result (nunion-with-copy x y)))
	(check-union x y result)))
  t)

(deftest nunion-6-a
    (let ((x '(a b c d e f))
	  (y '(z c y a v b)))
      (let ((result (nunion-with-copy x y :test #'eq)))
	(check-union x y result)))
  t)

(deftest nunion-7
    (let ((x '(a b c d e f))
	  (y '(z c y a v b)))
      (let ((result (nunion-with-copy x y :test #'eql)))
	(check-union x y result)))
  t)

(deftest nunion-8
    (let ((x '(a b c d e f))
	  (y '(z c y a v b)))
      (let ((result (nunion-with-copy x y :test #'equal)))
	(check-union x y result)))
  t)

(deftest nunion-9
    (let ((x  '(a b c d e f))
	  (y  '(z c y a v b)))
      (let ((result (nunion-with-copy x y :test-not (complement #'eql))))
	(check-union x y result)))
  t)

(deftest nunion-10
    (let ((x '(a b c d e f))
	  (y '(z c y a v b)))
      (let ((result (nunion-with-copy x y :test-not (complement #'equal))))
	(check-union x y result)))
  t)

(deftest nunion-11
    (let ((x '(a b c d e f))
	  (y '(z c y a v b)))
      (let ((result (nunion-with-copy x y :test-not (complement #'eq))))
	(check-union x y result)))
  t)

(deftest nunion-12
    (let ((x '(1 2 3 4 5 6 7))
	  (y '(10 19 5 3 17 1001 2)))
      (let ((result (nunion-with-copy x y)))
	(check-union x y result)))
  t)

(deftest nunion-13
    (let ((x '(1 2 3 4 5 6 7))
	  (y '(10 19 5 3 17 1001 2)))
      (let ((result (nunion-with-copy x y :test #'equal)))
	(check-union x y result)))
  t)

(deftest nunion-14
    (let ((x '(1 2 3 4 5 6 7))
	  (y '(10 19 5 3 17 1001 2)))
      (let ((result (nunion-with-copy x y :test #'eql)))
	(check-union x y result)))
  t)

(deftest nunion-15
    (let ((x '(1 2 3 4 5 6 7))
	  (y '(10 19 5 3 17 1001 2)))
      (let ((result (nunion-with-copy x y :test-not (complement #'equal))))
	(check-union x y result)))
  t)

(deftest nunion-16
    (let ((x '(1 2 3 4 5 6 7))
	  (y '(10 19 5 3 17 1001 2)))
      (let ((result (nunion-with-copy x y :test-not (complement  #'eql))))
	(check-union x y result)))
  t)

(deftest nunion-17
    (let ((x '(1 2 3 4 5 6 7))
	  (y '(10 19 5 3 17 1001 2)))
      (let ((result (nunion-with-copy-and-key x y #'1+)))
	(check-union x y result)))
  t)

(deftest nunion-18
    (let ((x '(1 2 3 4 5 6 7))
	  (y '(10 19 5 3 17 1001 2)))
      (let ((result (nunion-with-copy-and-key x y #'1+ :test #'equal)))
	(check-union x y result)))
  t)

(deftest nunion-19
    (let ((x '(1 2 3 4 5 6 7))
	  (y '(10 19 5 3 17 1001 2)))
      (let ((result (nunion-with-copy-and-key x y #'1+ :test #'eql)))
	(check-union x y result)))
  t)

(deftest nunion-20
    (let ((x '(1 2 3 4 5 6 7))
	  (y '(10 19 5 3 17 1001 2)))
      (let ((result (nunion-with-copy-and-key x y #'1+
					      :test-not (complement #'equal))))
	(check-union x y result)))
  t)

(deftest nunion-21
    (let ((x '(1 2 3 4 5 6 7))
	  (y '(10 19 5 3 17 1001 2)))
      (let ((result (nunion-with-copy-and-key x y #'1+
					      :test-not (complement #'equal))))
	(check-union x y result)))
  t)

(deftest nunion-22
  (handler-case
    (let ((x '(1 2 3 4 5 6 7))
	  (y '(10 19 5 3 17 1001 2)))
      (let ((result (nunion-with-copy-and-key x y nil)))
	(check-union x y result)))
    (error (c) c))
  t)

(deftest nunion-23
  (let ((x '(1 2 3 4 5 6 7))
	(y '(10 19 5 3 17 1001 2)))
    (let ((result (nunion-with-copy-and-key x y '1+)))
      (check-union x y result)))
  t)

;; Do large numbers of random units

(defun do-random-nunions (size niters &optional (maxelem (* 2 size)))
  (let ((state (make-random-state)))
    (loop
       for i from 1 to niters do
	  (let ((x (shuffle (loop for j from 1 to size collect
				  (random maxelem state))))
		(y (shuffle (loop for j from 1 to size collect
				  (random maxelem state)))))
	    (let ((z (nunion-with-copy x y)))
	      (let ((is-good (check-union x y z)))
		(unless is-good (return (values x y z)))))))
    nil))

(deftest nunion-24
  (do-random-nunions 100 100 200)
  nil)

(deftest nunion-25
  (let ((x (shuffle '(1 4 6 10 45 101)))
	(y '(102 5 2 11 44 6)))
    (let ((result (nunion-with-copy x y
				    :test #'(lambda (a b)
					      (<= (abs (- a b)) 1)))))
      (sort
       (sublis
	'((2 . 1) (5 . 4) (11 . 10) (45 . 44) (102 . 101))
	(copy-list result))
       #'<)))
  (1 4 6 10 44 101))

;; Check that nunion uses eql, not equal or eq

(deftest nunion-26
  (let ((x 1000)
	(y 1000))
    (loop
     while (not (typep x 'bignum))
     do (progn
	  (setf x (* x x))
	  (setf y (* y y))))
    (not (not
	  (or
	   (eq x y)  ;; if bignums are eq, the test is worthless
	   (eql (length
		 (nunion-with-copy (list x) (list x)))
		1)))))
  t)

(deftest nunion-27
  (nunion-with-copy (list (copy-seq "aa"))
		    (list (copy-seq "aa")))
  ("aa" "aa"))



;; Check that nunion does not reverse the arguments to :test, :test-not

(deftest nunion-28
    (block fail
      (sort
       (nunion-with-copy
	'(1 2 3)
	'(4 5 6)
	:test #'(lambda (x y)
		  (when (< y x) (return-from fail 'fail))
		  (eql x y)))
       #'<))
  (1 2 3 4 5 6))

(deftest nunion-29
    (block fail
      (sort
       (nunion-with-copy-and-key
	'(1 2 3)
	'(4 5 6)
	#'identity
	:test #'(lambda (x y)
		  (when (< y x) (return-from fail 'fail))
		  (eql x y)))
       #'<))
  (1 2 3 4 5 6))

(deftest nunion-30
    (block fail
      (sort
       (nunion-with-copy
	'(1 2 3)
	'(4 5 6)
	:test-not
	#'(lambda (x y)
	    (when (< y x) (return-from fail 'fail))
	    (not (eql x y))))
       #'<))
  (1 2 3 4 5 6))

(deftest nunion-31
    (block fail
      (sort
       (nunion-with-copy-and-key
	'(1 2 3)
	'(4 5 6)
	#'identity
	:test-not #'(lambda (x y)
		      (when (< y x) (return-from fail 'fail))
		      (not (eql x y))))
       #'<))
  (1 2 3 4 5 6))

