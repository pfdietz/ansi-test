;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon May  5 19:32:56 2003
;;;; Contains: Tests for UPDATE-INSTANCE-FOR-DIFFERENT-CLASS

(in-package :cl-test)

(defclass uifdc-class-01a () ((a :initarg :a) (b :initarg :b)))
(defclass uifdc-class-01b () (a b))

(defmethod update-instance-for-different-class
  ((from-obj uifdc-class-01a)
   (to-obj uifdc-class-01b)
   &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (when (slot-boundp from-obj 'a)
    (setf (slot-value to-obj 'b)
	  (slot-value from-obj 'a)))
  (when (slot-boundp from-obj 'b)
    (setf (slot-value to-obj 'a)
	  (slot-value from-obj 'b)))
  to-obj)

(deftest update-instance-for-different-class.1
  (let ((obj (make-instance 'uifdc-class-01a))
	(new-class (find-class 'uifdc-class-01b)))
    (values
     (map-slot-boundp* obj '(a b))
     (eqt obj (change-class obj new-class))
     (typep* obj new-class)
     (map-slot-boundp* obj '(a b))))
  (nil nil)
  t t
  (nil nil))

(deftest update-instance-for-different-class.2
  (let ((obj (make-instance 'uifdc-class-01a :a 1))
	(new-class (find-class 'uifdc-class-01b)))
    (values
     (map-slot-boundp* obj '(a b))
     (eqt obj (change-class obj new-class))
     (typep* obj new-class)
     (map-slot-boundp* obj '(a b))
     (slot-value obj 'a)
     (slot-value obj 'b)))
  (t nil)
  t t
  (t t)
  1 1)

(deftest update-instance-for-different-class.3
  (let ((obj (make-instance 'uifdc-class-01a :b 1))
	(new-class (find-class 'uifdc-class-01b)))
    (values
     (map-slot-boundp* obj '(a b))
     (eqt obj (change-class obj new-class))
     (typep* obj new-class)
     (map-slot-boundp* obj '(a b))
     (slot-value obj 'a)
     (slot-value obj 'b)))
  (nil t)
  t t
  (t t)
  1 1)

(deftest update-instance-for-different-class.4
  (let ((obj (make-instance 'uifdc-class-01a :a 1 :b 2))
	(new-class (find-class 'uifdc-class-01b)))
    (values
     (map-slot-boundp* obj '(a b))
     (eqt obj (change-class obj new-class))
     (typep* obj new-class)
     (map-slot-boundp* obj '(a b))
     (slot-value obj 'a)
     (slot-value obj 'b)))
  (t t)
  t t
  (t t)
  2 1)




