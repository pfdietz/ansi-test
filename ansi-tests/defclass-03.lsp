;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 27 16:23:59 2003
;;;; Contains: Tests of DEFCLASS with more involved inheritance

(in-package :cl-test)

;;;

(defclass class-0301a ()
  (a b))

(defclass class-0301b ()
  (a c))

(defclass class-0301c (class-0301a class-0301b)
  (d))

(deftest class-0301.1
  (let ((c (make-instance 'class-0301c)))
    (values
     (typep* c 'class-0301a)
     (typep* c 'class-0301b)
     (typep* c 'class-0301c)
     (typep* c (find-class 'class-0301a))
     (typep* c (find-class 'class-0301b))
     (typep* c (find-class 'class-0301c))
     (map-slot-boundp* c '(a b c d))
     (setf (slot-value c 'a) 'w)
     (setf (slot-value c 'b) 'x)
     (setf (slot-value c 'c) 'y)
     (setf (slot-value c 'd) 'z)
     (map-slot-boundp* c '(a b c d))
     (map-slot-value c '(a b c d))))
  t t t
  t t t
  (nil nil nil nil)
  w x y z
  (t t t t)
  (w x y z))

;;;

(defclass class-0302a ()
  ((a :initform 'x) b (c :initform 'w)))

(defclass class-0302b ()
  ((a :initform 'y) (b :initform 'z)))

(defclass class-0302c (class-0302a class-0302b)
  (a b (c :initform 'v) d))

(deftest class-0302.1
  (let ((c (make-instance 'class-0302c)))
    (values
     (map-slot-boundp* c '(a b c d))
     (map-slot-value c '(a b c))))
  (t t t nil)
  (x z v))

;;;

(defclass class-0303a ()
  ((a :allocation :class) b))

(defclass class-0303b ()
  (a (b :allocation :class)))

(defclass class-0303c (class-0303a class-0303b) ())

(deftest class-0303.1
  (let ((c1 (make-instance 'class-0303a))
	(c2 (make-instance 'class-0303b))
	(c3 (make-instance 'class-0303c)))
    (slot-makunbound c1 'a)
    (slot-makunbound c2 'b)
    (values
     (loop for c in (list c1 c2 c3)
	   collect (map-slot-boundp* c '(a b)))
     (list (setf (slot-value c1 'a) 'x1)
	   (slot-boundp* c2 'a)
	   (slot-value c3 'a))
     (list (setf (slot-value c2 'a) 'x2)
	   (slot-value c1 'a)
	   (slot-value c2 'a)
	   (slot-value c3 'a))
     (list (setf (slot-value c3 'a) 'x3)
	   (slot-value c1 'a)
	   (slot-value c2 'a)
	   (slot-value c3 'a))
     ;;;
     (list (setf (slot-value c1 'b) 'y1)
	   (slot-value c1 'b)
	   (slot-boundp* c2 'b)
	   (slot-boundp* c3 'b))
     (list (setf (slot-value c2 'b) 'y2)
	   (slot-value c1 'b)
	   (slot-value c2 'b)
	   (slot-boundp c3 'b))
     (list (setf (slot-value c3 'b) 'y3)
	   (slot-value c1 'b)
	   (slot-value c2 'b)
	   (slot-value c3 'b))))
  ((nil nil) (nil nil) (nil nil))
  (x1 nil x1)
  (x2 x1 x2 x1)
  (x3 x3 x2 x3)
  ;;
  (y1 y1 nil nil)
  (y2 y1 y2 nil)
  (y3 y1 y2 y3))

;;;

(defclass class-0304a ()
  ((a :initform 'x)))

(defclass class-0304b (class-0304a) ())

(defclass class-0304c (class-0304a)
  ((a :initform 'y)))

(defclass class-0304d (class-0304b class-0304c)
  ())

(deftest class-0304.1
  (slot-value (make-instance 'class-0304d) 'a)
  y)

;;;

(defclass class-0305a ()
  ((a :initarg :a))
  (:default-initargs :a 'x))

(defclass class-0305b (class-0305a) ())

(defclass class-0305c (class-0305a)
  ()
  (:default-initargs :a 'y))

(defclass class-0305d (class-0305b class-0305c)
  ())

(deftest class-0305.1
  (slot-value (make-instance 'class-0305d) 'a)
  y)


