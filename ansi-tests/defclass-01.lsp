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

(defclass class-07 () ((s1 :initarg :s1a :initarg :s1b :reader s1)
		       (s2 :initarg :s2 :reader s2)))

(deftest class-07.1
  (let ((c (make-instance 'class-07)))
    (values
     (slot-boundp c 's1)
     (slot-boundp c 's2)))
  nil nil)

(deftest class-07.2
  (let ((c (make-instance 'class-07 :s1a 'x)))
    (values
     (notnot (slot-boundp c 's1))
     (s1 c)
     (slot-boundp c 's2)))
  t x nil)

(deftest class-07.3
  (let ((c (make-instance 'class-07 :s1b 'x)))
    (values
     (notnot (slot-boundp c 's1))
     (s1 c)
     (slot-boundp c 's2)))
  t x nil)

(deftest class-07.4
  (let ((c (make-instance 'class-07 :s1a 'y :s1b 'x)))
    (values
     (notnot (slot-boundp c 's1))
     (s1 c)
     (slot-boundp c 's2)))
  t y nil)


(deftest class-07.5
  (let ((c (make-instance 'class-07 :s1b 'y :s1a 'x)))
    (values
     (notnot (slot-boundp c 's1))
     (s1 c)
     (slot-boundp c 's2)))
  t y nil)

(deftest class-07.6
  (let ((c (make-instance 'class-07 :s1a 'y :s1a 'x)))
    (values
     (notnot (slot-boundp c 's1))
     (s1 c)
     (slot-boundp c 's2)))
  t y nil)

(deftest class-07.7
  (let ((c (make-instance 'class-07 :s2 'a :s1a 'b)))
    (values
     (notnot (slot-boundp c 's1))
     (notnot (slot-boundp c 's2))
     (s1 c)
     (s2 c)))
  t t b a)

(deftest class-07.8
  (let ((c (make-instance 'class-07 :s2 'a :s1a 'b :s2 'x :s1a 'y :s1b 'z)))
    (values
     (notnot (slot-boundp c 's1))
     (notnot (slot-boundp c 's2))
     (s1 c)
     (s2 c)))
  t t b a)

(deftest class-07.9
  (let ((c (make-instance 'class-07 :s1b 'x :s1a 'y)))
    (values
     (notnot (slot-boundp c 's1))
     (slot-boundp c 's2)
     (s1 c)))
  t nil x)

;;;;

(declaim (special *class-08-s2-initvar*))

(defclass class-08 ()
  ((s1 :initform 0) (s2 :initform *class-08-s2-initvar*)))

(deftest class-08.1
  (let* ((*class-08-s2-initvar* 'x)
	 (c (make-instance 'class-08)))
    (values
     (slot-value c 's1)
     (slot-value c 's2)))
  0 x)

;;;;

(declaim (special *class-09-s2-initvar*))

(defclass class-09 ()
  ((s1 :initform 0 :initarg :s1)
   (s2 :initform *class-09-s2-initvar* :initarg :s2)))

(deftest class-09.1
  (let* ((*class-09-s2-initvar* 'x)
	 (c (make-instance 'class-09)))
    (values
     (slot-value c 's1)
     (slot-value c 's2)))
  0 x)

(deftest class-09.2
  (let* ((*class-09-s2-initvar* 'x)
	 (c (make-instance 'class-09 :s1 1)))
    (values
     (slot-value c 's1)
     (slot-value c 's2)))
  1 x)

(deftest class-09.3
  (let* ((c (make-instance 'class-09 :s2 'a)))
    (values
     (slot-value c 's1)
     (slot-value c 's2)))
  0 a)

(deftest class-09.4
  (let* ((c (make-instance 'class-09 :s2 'a :s1 10 :s1 'bad :s2 'bad)))
    (values
     (slot-value c 's1)
     (slot-value c 's2)))
  10 a)

;;;;

(declaim (special *class-10-s1-initvar*))

(defclass class-10 ()
  ((s1 :initform (incf *class-10-s1-initvar*) :initarg :s1)))

(deftest class-10.1
  (let* ((*class-10-s1-initvar* 0)
	 (c (make-instance 'class-10)))
    (values
     *class-10-s1-initvar*
     (slot-value c 's1)))
  1 1)

(deftest class-10.2
  (let* ((*class-10-s1-initvar* 0)
	 (c (make-instance 'class-10 :s1 10)))
    (values
     *class-10-s1-initvar*
     (slot-value c 's1)))
  0 10)

;;;;

(let ((x 7))
  (defclass class-11 ()
    ((s1 :initform x :initarg :s1))))

(deftest class-11.1
  (slot-value (make-instance 'class-11) 's1)
  7)

(deftest class-11.2
  (slot-value (make-instance 'class-11 :s1 100) 's1)
  100)

;;;

(flet ((%f () 'x))
  (defclass class-12 ()
    ((s1 :initform (%f) :initarg :s1))))

(deftest class-12.1
  (slot-value (make-instance 'class-12) 's1)
  x)

(deftest class-12.2
  (slot-value (make-instance 'class-12 :s1 'y) 's1)
  y)



    
    