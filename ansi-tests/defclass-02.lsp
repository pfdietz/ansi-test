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
     (eq (slot-makunbound c2 'a) c1)
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

  
     