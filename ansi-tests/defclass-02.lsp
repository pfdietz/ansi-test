;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Apr 25 07:16:57 2003
;;;; Contains: Tests of DEFCLASS with simple inheritance

(in-package :cl-test)

;;;

(defclass class-0201 ()
  ((a :initform 'x) (b :allocation :instance) (c :reader class-0201-c)))

(defclass class-0202 (class-0201)
  (d (e :initform 'y) (f :allocation :instance)))

(deftest class-0201.1
  (let ((c (make-instance 'class-0201)))
    (values (map-slot-boundp* c '(a b c))
	    (map-slot-exists-p* c '(a b c))
	    (slot-value c 'a)
	    (map-typep* c (list 'class-0201 'class-0202
				(find-class 'class-0201)
				(find-class 'class-0202)))
	    (class-name (class-of c))
	    ))
  (t nil nil)
  (t t t)
  x
  (t nil t nil)
  class-0201)

(deftest class-0202.1
  (let ((c (make-instance 'class-0202)))
    (values (map-slot-boundp* c '(a b c d e f))
	    (map-slot-value c '(a e))
	    (map-typep* c (list 'class-0201 'class-0202
				(find-class 'class-0201)
				(find-class 'class-0202)))
	    (class-name (class-of c))
	    ))
  (t nil nil nil t nil)
  (x y)
  (t t t t)
  class-0202)

;;;


(defclass class-0203 ()
  ((a :allocation :class) (b :allocation :instance)))

(defclass class-0204 (class-0203)
  (c d))

(deftest class-0203.1
  (let ((c1 (make-instance 'class-0203))
	(c2 (make-instance 'class-0204)))
    (values
     (map-slot-boundp* c1 '(a b))
     (map-slot-boundp* c2 '(a b c d))
     (setf (slot-value c1 'a) 'x)
     (map-slot-boundp* c1 '(a b))
     (map-slot-boundp* c2 '(a b c d))
     (slot-value c1 'a)
     (slot-value c2 'a)
     (eq (slot-makunbound c1 'a) c1)
     (map-slot-boundp* c1 '(a b))
     (map-slot-boundp* c2 '(a b c d))))
  (nil nil)
  (nil nil nil nil)
  x
  (t nil)
  (t nil nil nil)
  x x
  t
  (nil nil)
  (nil nil nil nil))

  
(deftest class-0203.2
  (let ((c1 (make-instance 'class-0203))
	(c2 (make-instance 'class-0204)))
    (values
     (map-slot-boundp* c1 '(a b))
     (map-slot-boundp* c2 '(a b c d))
     (setf (slot-value c1 'a) 'x)
     (map-slot-boundp* c1 '(a b))
     (map-slot-boundp* c2 '(a b c d))
     (slot-value c1 'a)
     (slot-value c2 'a)
     (eq (slot-makunbound c2 'a) c2)
     (map-slot-boundp* c1 '(a b))
     (map-slot-boundp* c2 '(a b c d))))
  (nil nil)
  (nil nil nil nil)
  x
  (t nil)
  (t nil nil nil)
  x x
  t
  (nil nil)
  (nil nil nil nil))

;;;

(defclass class-0205a ()
  ((a :initform 'x)
   (b :initform 'y)
   c))

(defclass class-0205b (class-0205a)
  ((a :initform 'z)
   b
   (c :initform 'w)))

(deftest class-0205a.1
  (let ((c (make-instance 'class-0205a)))
    (values
     (slot-value c 'a)
     (slot-value c 'b)
     (slot-boundp c 'c)))
  x y nil)

(deftest class-0205b.1
  (let ((c (make-instance 'class-0205b)))
    (map-slot-value c '(a b c)))
  (z y w))

;;;

(defclass class-0206a ()
  ((a :allocation :instance)
   (b :allocation :class)))

(defclass class-0206b (class-0206a)
  ((a :allocation :class)
   (b :allocation :instance)))

(deftest class-0206.1
  (let ((c1 (make-instance 'class-0206a))	
	(c2 (make-instance 'class-0206b)))
    (values
     (map-slot-boundp* c1 '(a b))
     (map-slot-boundp* c2 '(a b))
     (setf (slot-value c1 'a) 'x)
     (setf (slot-value c1 'b) 'y)
     (map-slot-boundp* c1 '(a b))
     (map-slot-boundp* c2 '(a b))
     (map-slot-value c1 '(a b))
     (progn (slot-makunbound c1 'a)
	    (slot-makunbound c1 'b)
	    (setf (slot-value c2 'a) 'x))
     (setf (slot-value c2 'b) 'y)
     (map-slot-boundp* c1 '(a b))
     (map-slot-boundp* c2 '(a b))
     (map-slot-value c2 '(a b))
     (progn (slot-makunbound c2 'a)
	    (slot-makunbound c2 'b)
	    nil)))
  (nil nil) (nil nil)
  x y
  (t t) (nil nil)
  (x y)
  x y
  (nil nil) (t t)
  (x y)
  nil)

;;;

;;; Show shadowing of slots by :allocation

(defclass class-0207a ()
  ((a :allocation :class)))

(defclass class-0207b (class-0207a)
  ((a :allocation :instance)))

(defclass class-0207c (class-0207b)
  ((a :allocation :class)))

(deftest class-0207.1
  (let ((c1 (make-instance 'class-0207a))
	(c2 (make-instance 'class-0207b))
	(c3 (make-instance 'class-0207c)))
    (slot-makunbound c1 'a)
    (slot-makunbound c2 'a)
    (slot-makunbound c3 'a)
    (values
     (setf (slot-value c1 'a) 'x)
     (slot-boundp* c1 'a)
     (slot-boundp* c2 'a)
     (slot-boundp* c3 'a)
     (slot-value c1 'a)
     (setf (slot-value c2 'a) 'y)
     (slot-boundp* c1 'a)
     (slot-boundp* c2 'a)
     (slot-boundp* c3 'a)
     (slot-value c1 'a)
     (slot-value c2 'a)
     (setf (slot-value c3 'a) 'z)
     (slot-boundp* c1 'a)
     (slot-boundp* c2 'a)
     (slot-boundp* c3 'a)
     (slot-value c1 'a)
     (slot-value c2 'a)
     (slot-value c3 'a)))
  x
  t nil nil
  x
  y
  t t nil
  x y
  z
  t t t
  x y z)

;;;

;;; Initforms are inherited even if :allocation changes

(defclass class-0208a ()
  ((a :allocation :class :initform 'x)))

(defclass class-0208b (class-0208a)
  ((a :allocation :instance)))

(deftest class-0208.1
  (values
   (slot-value (make-instance 'class-0208a) 'a)
   (slot-value (make-instance 'class-0208b) 'a))
  x x)
