;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 20:58:54 2003
;;;; Contains: Tests for DEFCLASS, part 01


(in-package :cl-test)

;;; I've decided to write some 'manual' tests, then refactor these back
;;; to the automatic mechanisms I'll put into defclass-aux.lsp after
;;; I have a better understanding of the object system

(defclass class-01 () (s1 s2 s3))

(deftest class-01.1
  (notnot-mv (typep (make-instance 'class-01) 'class-01))
  t)

(deftest class-01.2
  (notnot-mv (typep (make-instance (find-class 'class-01)) 'class-01))
  t)

(deftest class-01.3
  (let ((c (make-instance 'class-01)))
    (values
     (setf (slot-value c 's1) 12)
     (setf (slot-value c 's2) 18)
     (setf (slot-value c 's3) 27)
     (loop for s in '(s1 s2 s3) collect (slot-value c s))))
  12 18 27
  (12 18 27))

;;;;

(defclass class-02 () ((s1) (s2) (s3)))
  
(deftest class-02.1
  (notnot-mv (typep (make-instance 'class-02) 'class-02))
  t)

(deftest class-02.2
  (notnot-mv (typep (make-instance (find-class 'class-02)) 'class-02))
  t)

(deftest class-02.3
  (let ((c (make-instance 'class-02)))
    (values
     (setf (slot-value c 's1) 12)
     (setf (slot-value c 's2) 18)
     (setf (slot-value c 's3) 27)
     (loop for s in '(s1 s2 s3) collect (slot-value c s))))
  12 18 27
  (12 18 27))

;;;;

(defclass class-03 () ((s1 :type integer) (s2 :type t) (s3 :type fixnum)))
  
(deftest class-03.1
  (notnot-mv (typep (make-instance 'class-03) 'class-03))
  t)

(deftest class-03.2
  (notnot-mv (typep (make-instance (find-class 'class-03)) 'class-03))
  t)

(deftest class-03.3
  (let ((c (make-instance 'class-03)))
    (values
     (setf (slot-value c 's1) 12)
     (setf (slot-value c 's2) 'a)
     (setf (slot-value c 's3) 27)
     (loop for s in '(s1 s2 s3) collect (slot-value c s))))
  12 a 27
  (12 a 27))

;;;;

(defclass class-04 () ((s1 :reader s1-r) (s2 :writer s2-w) (s3 :accessor s3-a)))

;;; Readers, writers, and accessors
(deftest class-04.1
  (let ((c (make-instance 'class-04)))
    (values
     (setf (slot-value c 's1) 'a)
     (setf (slot-value c 's2) 'b)
     (setf (slot-value c 's3) 'c)
     (s1-r c)
     (slot-value c 's2)
     (s2-w 'd c)
     (slot-value c 's2)
     (s3-a c)
     (setf (s3-a c) 'e)
     (slot-value c 's3)
     (s3-a c)))
  a b c a b d d c e e e)

;;;;

(defclass class-05 () (s1 (s2 :allocation :instance) (s3 :allocation :class)))
  
(deftest class-05.1
  (let ((c1 (make-instance 'class-05))
	(c2 (make-instance 'class-05)))
    (values
     (not (eql c1 c2))
     (list
      (setf (slot-value c1 's1) 12)
      (setf (slot-value c2 's1) 17)
      (slot-value c1 's1)
      (slot-value c2 's1))
     (list
      (setf (slot-value c1 's2) 'a)
      (setf (slot-value c2 's2) 'b)
      (slot-value c1 's2)
      (slot-value c2 's2))
     (list
      (setf (slot-value c1 's3) 'x)
      (slot-value c1 's3)
      (slot-value c2 's3)
      (setf (slot-value c2 's3) 'y)
      (slot-value c1 's3)
      (slot-value c2 's3)
      (setf (slot-value c1 's3) 'z)
      (slot-value c1 's3)
      (slot-value c2 's3))
     (slot-value (make-instance 'class-05) 's3)))
  t
  (12 17 12 17)
  (a b a b)
  (x x x y y y z z z)
  z)

;;;;

(defclass class-06 () ((s1 :reader s1-r1 :reader s1-r2 :writer s1-w1 :writer s1-w2)))
(defclass class-06a () ((s1 :reader s1-r1) s3))

(deftest class-06.1
  (let ((c (make-instance 'class-06)))
    (values
     (setf (slot-value c 's1) 'x)
     (slot-value c 's1)
     (s1-r1 c)
     (s1-r2 c)
     (s1-w1 'y c)
     (slot-value c 's1)
     (s1-r1 c)
     (s1-r2 c)
     (s1-w2 'z c)
     (slot-value c 's1)
     (s1-r1 c)
     (s1-r2 c)))
  x x x x y y y y z z z z)

(deftest class-06.2
  (let ((c1 (make-instance 'class-06))
	(c2 (make-instance 'class-06a)))
    (values
     (setf (slot-value c1 's1) 'x)
     (setf (slot-value c2 's1) 'y)
     (mapcar #'s1-r1 (list c1 c2))))
  x y (x y))

;;;;


     



