;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Apr  8 20:05:24 1998
;;;; Contains: Test cases for CL, section 14.1 of CLtL2, Lists as Sequences

(in-package :cl-test)
(use-package :rt)

(declaim (optimize (safety 3)))

(defun safe-elt (x n)
  (handler-case
   (elt x n)
   (type-error () 'type-error)
   (error (c) c)))

;; elt on lists

(deftest elt-1 (safe-elt nil 0) type-error)
(deftest elt-1a (safe-elt nil -10) type-error)
(deftest elt-2 (safe-elt nil 1000000) type-error)
(deftest elt-3 (safe-elt '(a b c d e) 0) a)
(deftest elt-4 (safe-elt '(a b c d e) 2) c)
(deftest elt-5 (safe-elt '(a b c d e) 4) e)
(deftest elt-5a (safe-elt '(a b c d e) -4) type-error)
(deftest elt-6
  (let ((x (make-int-list 1000)))
    (not (not
	  (every
	   #'(lambda (i)
	       (eql i (safe-elt x i)))
	   x))))
  t)

(deftest elt-7
  (let* ((x (list 'a 'b 'c 'd))
	 (y (setf (elt x 0) 'e)))
    (list x y))
  ((e b c d) e))

(deftest elt-8
  (let* ((x (list 'a 'b 'c 'd))
	 (y (setf (elt x 1) 'e)))
    (list x y))
  ((a e c d) e))

(deftest elt-9
  (let* ((x (list 'a 'b 'c 'd))
	 (y (setf (elt x 3) 'e)))
    (list x y))
  ((a b c e) e))

(deftest elt-10
  (handler-case
   (let ((x (list 'a 'b 'c)))
     (setf (elt x 4) 'd))
   (type-error () 'type-error)
   (error (c) c))
  type-error)

(deftest elt-11
  (let ((x (list 'a 'b 'c 'd 'e)))
    (let ((y (loop for c on x collect c)))
      (setf (elt x 2) 'f)
      (not
       (not
	(every #'eq
	       y
	       (loop for c on x collect c))))))
  t)

(deftest elt-12
  (let ((x (make-int-list 100000)))
    (safe-elt x 90000))
  90000)

(deftest elt-13
  (let ((x (make-int-list 100000)))
    (setf (elt x 80000) 'foo)
    (list (safe-elt x 79999)
	  (safe-elt x 80000)
	  (safe-elt x 80001)))
  (79999 foo 80001))


;; Special case to test error handling as dictated by new
;; CL standard
(deftest elt-14
  (let ((x (list 'a 'b 'c)))
    (safe-elt x 10))
  type-error)

(deftest elt-15
  (let ((x (list 'a 'b 'c)))
    (safe-elt x 'a))
  type-error)

(deftest elt-16
  (let ((x (list 'a 'b 'c)))
    (safe-elt x 10.0))
  type-error)

(deftest elt-17
  (let ((x (list 'a 'b 'c)))
    (safe-elt x -1))
  type-error)

(deftest elt-18
  (let ((x (list 'a 'b 'c)))
    (safe-elt x -100000000000000000))
  type-error)

(deftest elt-19
  (let ((x (list 'a 'b 'c)))
    (safe-elt x #\w))
  type-error)

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
	    (when (or (not (eq g e))
		      (not (eql (car e) (car f)))
		      (car e)
		      (eq e f))
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

;; length of lists

(deftest length-list-1
    (length ())
  0)

(deftest length-list-2
    (length '(a b c d e f g))
  7)

(deftest length-list-3
    (length (make-list 200000))
  200000)

(defun length-list-4-body ()

    (let ((x ()))
      (loop
	  for i from 0 to 999 do
	    (progn
	      (unless (eql (length x) i) (return nil))
	      (push i x))
	  finally (return t))))

(deftest length-list-4
    (length-list-4-body)
  t)

;; reverse of lists

(deftest reverse-list-1
    (reverse ())
  ())

(deftest reverse-list-2
    (reverse '(a b c d e f))
  (f e d c b a))

(defun reverse-list-3-body ()
    (let* ((x (loop for i from 0 to 999 collect i))
	   (z (loop for e on x collect e))
	   (y (reverse x)))
      (and
       (loop
	   for e on x and f in z do
	     (unless (eq e f) (return nil))
	   finally (return t))
       (every #'eql
	      y
	      (loop for i from 999 downto 0 collect i)))))

(deftest reverse-list-3
    (reverse-list-3-body)
  t)

(deftest nreverse-list-1
    (nreverse nil)
  nil)

(deftest nreverse-list-2
    (nreverse (list 'a))
  (a))

(deftest nreverse-list-3
    (nreverse (list 'a 'b 'c 'd))
  (d c b a))

(defun nreverse-list-4-body ()
  (let* ((x (loop for i from 0 to 100000 collect i))
	 (y (copy-seq x)))
    (equal y (nreverse (nreverse x)))))

(deftest nreverse-list-4
    (nreverse-list-4-body)
  t)



(deftest make-sequence-list-1
    (make-sequence 'list 0)
  nil)

(deftest make-sequence-list-2
    (make-sequence 'list 5 :initial-element 'g)
  (g g g g g))

(deftest make-sequence-list-3
    (length (make-sequence 'list 100))
  100)

(deftest make-sequence-list-4
    (length (make-sequence 'list 123456))
  123456)

(deftest make-sequence-list-5
    (let ((x (make-sequence 'list 1000)))
      (every #'(lambda (e) (eq (car x) e)) (cdr x)))
  t)
