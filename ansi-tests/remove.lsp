;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Sep 14 11:46:05 2002
;;;; Contains: Tests for REMOVE

(in-package :cl-test)

(deftest remove-list.1
  (let* ((orig '(a b c a b d a c b a e))
	 (x (copy-seq orig))
	 (y (remove 'a x)))
    (and (equalp orig x) y))
  (b c b d c b e))

(deftest remove-list.2
  (let* ((orig '(a b c a b d a c b a e))
	 (x (copy-seq orig))
	 (y (remove 'a x :count nil)))
    (and (equalp orig x) y))
  (b c b d c b e))

(deftest remove-list.3
  (let* ((orig '(a b c a b d a c b a e))
	 (x (copy-seq orig))
	 (y (remove 'a x :key nil)))
    (and (equalp orig x) y))
  (b c b d c b e))

(deftest remove-list.4
  (let* ((orig '(a b c a b d a c b a e))
	 (x (copy-seq orig))
	 (y (remove 'a x :count 100)))
    (and (equalp orig x) y))
  (b c b d c b e))

(deftest remove-list.5
  (let* ((orig '(a b c a b d a c b a e))
	 (x (copy-seq orig))
	 (y (remove 'a x :count 0)))
    (and (equalp orig x) y))
  (a b c a b d a c b a e))

(deftest remove-list.6
  (let* ((orig '(a b c a b d a c b a e))
	 (x (copy-seq orig))
	 (y (remove 'a x :count 1)))
    (and (equalp orig x) y))
  (b c a b d a c b a e))

(deftest remove-list.7
  (let* ((orig '(a b c a b d a c b a e))
	 (x (copy-seq orig))
	 (y (remove 'c x :count 1)))
    (and (equalp orig x) y))
  (a b a b d a c b a e))

(deftest remove-list.8
  (let* ((orig '(a b c a b d a c b a e))
	 (x (copy-seq orig))
	 (y (remove 'a x :from-end t)))
    (and (equalp orig x) y))
  (b c b d c b e))

(deftest remove-list.9
  (let* ((orig '(a b c a b d a c b a e))
	 (x (copy-seq orig))
	 (y (remove 'a x :from-end t :count 1)))
    (and (equalp orig x) y))
  (a b c a b d a c b e))

(deftest remove-list.10
  (let* ((orig '(a b c a b d a c b a e))
	 (x (copy-seq orig))
	 (y (remove 'a x :from-end t :count 4)))
    (and (equalp orig x) y))
  (b c b d c b e))

(deftest remove-list.11
  (let* ((orig '(a b c a b d a c b a e))
	 (x (copy-seq orig)))
    (values
     (loop for i from 0 to 10
	   collect (remove 'a x :start i))
     (equalp orig x)))
  ((b c b d c b e)
   (a b c b d c b e)
   (a b c b d c b e)
   (a b c b d c b e)
   (a b c a b d c b e)
   (a b c a b d c b e)
   (a b c a b d c b e)
   (a b c a b d a c b e)
   (a b c a b d a c b e)
   (a b c a b d a c b e)
   (a b c a b d a c b a e))
  t)  

(deftest remove-list.12
 (let* ((orig '(a b c a b d a c b a e))
	 (x (copy-seq orig)))
    (values
     (loop for i from 0 to 10
	   collect (remove 'a x :start i :end nil))
     (equalp orig x)))
  ((b c b d c b e)
   (a b c b d c b e)
   (a b c b d c b e)
   (a b c b d c b e)
   (a b c a b d c b e)
   (a b c a b d c b e)
   (a b c a b d c b e)
   (a b c a b d a c b e)
   (a b c a b d a c b e)
   (a b c a b d a c b e)
   (a b c a b d a c b a e))
  t)

(deftest remove-list.13
 (let* ((orig '(a b c a b d a c b a e))
	 (x (copy-seq orig)))
    (values
     (loop for i from 0 to 10
	   collect (remove 'a x :start i :end 11))
     (equalp orig x)))
  ((b c b d c b e)
   (a b c b d c b e)
   (a b c b d c b e)
   (a b c b d c b e)
   (a b c a b d c b e)
   (a b c a b d c b e)
   (a b c a b d c b e)
   (a b c a b d a c b e)
   (a b c a b d a c b e)
   (a b c a b d a c b e)
   (a b c a b d a c b a e))
  t)

(deftest remove-list.14
  (let* ((orig '(a b c a b d a c b a e))
	 (x (copy-seq orig))
	 (y (remove 'a x :end nil)))
    (and (equalp orig x) y))
  (b c b d c b e))
 
(deftest remove-list.15
 (let* ((orig '(a b c a b d a c b a e))
	 (x (copy-seq orig)))
    (values
     (loop for i from 0 to 9
	   collect (remove 'a x :start i :end 9))
     (equalp orig x)))
  ((b c b d c b a e)
   (a b c b d c b a e)
   (a b c b d c b a e)
   (a b c b d c b a e)
   (a b c a b d c b a e)
   (a b c a b d c b a e)
   (a b c a b d c b a e)
   (a b c a b d a c b a e)
   (a b c a b d a c b a e)
   (a b c a b d a c b a e))
  t)

(deftest remove-list.16
 (let* ((orig '(a b c a b d a c b a e))
	 (x (copy-seq orig)))
    (values
     (loop for i from 0 to 10
	   collect (remove 'a x :start i :end 11 :count 1))
     (equalp orig x)))
 ((b c a b d a c b a e)
  (a b c b d a c b a e)
  (a b c b d a c b a e)
  (a b c b d a c b a e)
  (a b c a b d c b a e)
  (a b c a b d c b a e)
  (a b c a b d c b a e)
  (a b c a b d a c b e)
  (a b c a b d a c b e)
  (a b c a b d a c b e)
  (a b c a b d a c b a e))
 t)

(deftest remove-list.17
 (let* ((orig '(a b c a b d a c b a e))
	 (x (copy-seq orig)))
    (values
     (loop for i from 0 to 10
	   collect (remove 'a x :start i :end (1+ i)))
     (equalp orig x)))
 ((  b c a b d a c b a e)
  (a b c a b d a c b a e)
  (a b c a b d a c b a e)
  (a b c   b d a c b a e)
  (a b c a b d a c b a e)
  (a b c a b d a c b a e)
  (a b c a b d   c b a e)
  (a b c a b d a c b a e)
  (a b c a b d a c b a e)
  (a b c a b d a c b   e)
  (a b c a b d a c b a e))
 t)

;;; Show that it tests using EQL, not EQ
(deftest remove-list.18
   (let* ((i (1+ most-positive-fixnum))
	  (orig (list i 0 i 1 i 2 3))
	  (x (copy-seq orig))
	  (y (remove (1+ most-positive-fixnum) x)))
     (and (equalp orig x) y))
   (0 1 2 3))

(deftest remove-list.19
  (let* ((orig '(1 2 3 2 6 1 2 4 1 3 2 7))
	 (x (copy-seq orig))
	 (y (remove 1 x :key #'1-)))
    (and (equalp orig x) y))
  (1 3 6 1 4 1 3 7))

(deftest remove-list.20
  (let* ((orig '(1 2 3 2 6 1 2 4 1 3 2 7))
	 (x (copy-seq orig))
	 (y (remove 3 x :test #'>)))
    (and (equalp orig x) y))
  (3 6 4 3 7))

(deftest remove-list.21
  (let* ((orig '(1 2 3 2 6 1 2 4 1 3 2 7))
	 (x (copy-seq orig))
	 (y (remove 3 x :test '> :from-end t)))
    (and (equalp orig x) y))
  (3 6 4 3 7))

(deftest remove-list.22
  (let* ((orig '(1 2 3 2 6 1 2 4 1 3 2 7))
	 (x (copy-seq orig))
	 (y (remove 2 x :key nil)))
    (and (equalp orig x) y))
  (1 3 6 1 4 1 3 7))

(deftest remove-list.23
  (let* ((orig '(1 2 3 2 6 1 2 4 1 3 2 7))
	 (x (copy-seq orig))
	 (y (remove 1 x :key '1-)))
    (and (equalp orig x) y))
  (1 3 6 1 4 1 3 7))

(deftest remove-list.24
  (let* ((orig '(1 2 3 2 6 1 2 4 1 3 2 7))
	 (x (copy-seq orig))
	 (y (remove 3 x :test-not #'<=)))
    (and (equalp orig x) y))
  (3 6 4 3 7))

(deftest remove-list.25
  (let* ((orig '(1 2 3 2 6 1 2 4 1 3 2 7))
	 (x (copy-seq orig))
	 (y (remove 3 x :test-not '<= :from-end t)))
    (and (equalp orig x) y))
  (3 6 4 3 7))

(deftest remove-list.26
  (let* ((orig '(1 2 3 2 6 1 2 4 1 3 2 7))
	 (x (copy-seq orig))
	 (y (remove 3 x :from-end t :start 1 :end 5)))
    (and (equalp orig x) y))
  (1 2 2 6 1 2 4 1 3 2 7))

;;; Randomized tests

(deftest remove-random
  (loop for i from 1 to 2500
	always (random-test-remove 20))
  t)

(deftest remove-if-random
  (loop for i from 1 to 2500
	always (random-test-remove-if 20))
  t)

(deftest remove-if-not-random
  (loop for i from 1 to 2500
	always (random-test-remove-if 20 t))
  t)

(deftest delete-random
  (loop for i from 1 to 2500
	always (random-test-delete 20))
  t)

(deftest delete-if-random
  (loop for i from 1 to 2500
	always (random-test-delete-if 20))
  t)

(deftest delete-if-not-random
  (loop for i from 1 to 2500
	always (random-test-delete-if 20 t))
  t)

;;; Additional tests with KEY = NIL

(deftest remove-if-list.1
  (let* ((orig '(1 2 3 2 6 1 2 4 1 3 2 7))
	 (x (copy-seq orig))
	 (y (remove-if #'evenp x :key nil)))
    (and (equalp orig x) y))
  (1 3 1 1 3 7))

(deftest remove-if-list.2
  (let* ((orig '(a b c a b d a c b a e))
	 (x (copy-seq orig))
	 (y (remove-if #'(lambda (y) (eqt y 'a)) x :key nil)))
    (and (equalp orig x) y))
  (b c b d c b e))

(deftest remove-if-not-list.1
  (let* ((orig '(1 2 3 2 6 1 2 4 1 3 2 7))
	 (x (copy-seq orig))
	 (y (remove-if-not #'oddp x :key nil)))
    (and (equalp orig x) y))
  (1 3 1 1 3 7))

(deftest remove-if-not-list.2
  (let* ((orig '(a b c a b d a c b a e))
	 (x (copy-seq orig))
	 (y (remove-if-not #'(lambda (y) (not (eqt y 'a))) x :key nil)))
    (and (equalp orig x) y))
  (b c b d c b e))

(deftest delete-if-list.1
  (let* ((orig '(1 2 3 2 6 1 2 4 1 3 2 7))
	 (x (copy-seq orig))
	 (y (delete-if #'evenp x :key nil)))
    y)
  (1 3 1 1 3 7))

(deftest delete-if-list.2
  (let* ((orig '(a b c a b d a c b a e))
	 (x (copy-seq orig))
	 (y (delete-if #'(lambda (y) (eqt y 'a)) x :key nil)))
    y)
  (b c b d c b e))

(deftest delete-if-not-list.1
  (let* ((orig '(1 2 3 2 6 1 2 4 1 3 2 7))
	 (x (copy-seq orig))
	 (y (delete-if-not #'oddp x :key nil)))
    y)
  (1 3 1 1 3 7))

(deftest delete-if-not-list.2
  (let* ((orig '(a b c a b d a c b a e))
	 (x (copy-seq orig))
	 (y (delete-if-not #'(lambda (y) (not (eqt y 'a))) x :key nil)))
    y)
  (b c b d c b e))

(deftest delete-list.1
  (let* ((orig '(a b c a b d a c b a e))
	 (x (copy-seq orig))
	 (y (delete 'a x :key nil)))
    y)
  (b c b d c b e))

(deftest delete-list.2
  (let* ((orig '(1 2 3 2 6 1 2 4 1 3 2 7))
	 (x (copy-seq orig))
	 (y (delete 2 x :key nil)))
    y)
  (1 3 6 1 4 1 3 7))
