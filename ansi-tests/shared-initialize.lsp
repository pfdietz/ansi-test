;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Apr 29 04:09:06 2003
;;;; Contains: Tests of SHARED-INITIALIZE

(in-package :cl-test)

(defclass shared-init-class-01 ()
  ((a :initform 'x :initarg :a)
   (b :initform 'y :initarg :b)
   (c :initarg :c)
   d))

(deftest shared-initialize.1
  (let ((obj (allocate-instance (find-class 'shared-init-class-01))))
    (values
     (map-slot-boundp* obj '(a b c d))
     (eqt obj (shared-initialize obj nil :a 1 :b 3 :c 14))
     (map-slot-boundp* obj '(a b c d))
     (map-slot-value obj '(a b c))))
  (nil nil nil nil)
  t
  (t t t nil)
  (1 3 14))

(deftest shared-initialize.2
  (let ((obj (allocate-instance (find-class 'shared-init-class-01))))
    (values
     (map-slot-boundp* obj '(a b c d))
     (eqt obj (shared-initialize obj nil))
     (map-slot-boundp* obj '(a b c d))))
  (nil nil nil nil)
  t
  (nil nil nil nil))

(deftest shared-initialize.3
  (let ((obj (allocate-instance (find-class 'shared-init-class-01))))
    (values
     (map-slot-boundp* obj '(a b c d))
     (eqt obj (shared-initialize obj nil :a 1 :a 2))
     (map-slot-boundp* obj '(a b c d))
     (slot-value obj 'a)))
  (nil nil nil nil)
  t
  (t nil nil nil)
  1)

(deftest shared-initialize.4
  (let ((obj (allocate-instance (find-class 'shared-init-class-01))))
    (values
     (map-slot-boundp* obj '(a b c d))
     (eqt obj (shared-initialize obj nil :a 1 :a 2 :allow-other-keys nil))
     (map-slot-boundp* obj '(a b c d))
     (slot-value obj 'a)))
  (nil nil nil nil)
  t
  (t nil nil nil)
  1)

(deftest shared-initialize.5
  (let ((obj (allocate-instance (find-class 'shared-init-class-01))))
    (values
     (map-slot-boundp* obj '(a b c d))
     (eqt obj (shared-initialize obj '(a) :a 1))
     (map-slot-boundp* obj '(a b c d))
     (slot-value obj 'a)))
  (nil nil nil nil)
  t
  (t nil nil nil)
  1)

(deftest shared-initialize.6
  (let ((obj (allocate-instance (find-class 'shared-init-class-01))))
    (values
     (map-slot-boundp* obj '(a b c d))
     (eqt obj (shared-initialize obj '(a)))
     (map-slot-boundp* obj '(a b c d))
     (slot-value obj 'a)))
  (nil nil nil nil)
  t
  (t nil nil nil)
  x)

(deftest shared-initialize.7
  (let ((obj (allocate-instance (find-class 'shared-init-class-01))))
    (values
     (map-slot-boundp* obj '(a b c d))
     (eqt obj (shared-initialize obj t))
     (map-slot-boundp* obj '(a b c d))
     (slot-value obj 'a)
     (slot-value obj 'b)))
  (nil nil nil nil)
  t
  (t t nil nil)
  x y)

(deftest shared-initialize.8
  (let ((obj (allocate-instance (find-class 'shared-init-class-01))))
    (values
     (map-slot-boundp* obj '(a b c d))
     (eqt obj (shared-initialize obj t :b 10 :c 100))
     (map-slot-boundp* obj '(a b c d))
     (slot-value obj 'a)
     (slot-value obj 'b)
     (slot-value obj 'c)))
  (nil nil nil nil)
  t
  (t t t nil)
  x 10 100)

(deftest shared-initialize.9
  (let ((obj (allocate-instance (find-class 'shared-init-class-01))))
    (values
     (map-slot-boundp* obj '(a b c d))
     (eqt obj (shared-initialize obj nil :a 1 :b 10 :c 100))
     (eqt obj (shared-initialize obj nil :a 5 :b 37 :c 213))
     (map-slot-boundp* obj '(a b c d))
     (slot-value obj 'a)
     (slot-value obj 'b)
     (slot-value obj 'c)))
  (nil nil nil nil)
  t t
  (t t t nil)
  5 37 213)



