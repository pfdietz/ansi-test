;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 10 09:39:01 2003
;;;; Contains: Tests of SLOT-EXISTS-P

(in-package :cl-test)

;;; This function is also tested incidentally in many other files

(defclass slot-exists-p-class-01 ()
  (a (b :allocation :class) (c :allocation :instance)))

(deftest slot-exists-p.1
  (let ((obj (allocate-instance (find-class 'slot-exists-p-class-01))))
    (notnot-mv (slot-exists-p obj 'a)))
  t)
     
(deftest slot-exists-p.2
  (let ((obj (allocate-instance (find-class 'slot-exists-p-class-01))))
    (notnot-mv (slot-exists-p obj 'b)))
  t)
     
(deftest slot-exists-p.3
  (let ((obj (allocate-instance (find-class 'slot-exists-p-class-01))))
    (notnot-mv (slot-exists-p obj 'c)))
  t)

(deftest slot-exists-p.4
  (let ((obj (allocate-instance (find-class 'slot-exists-p-class-01))))
    (slot-exists-p obj 'd))
  nil)

(deftest slot-exists-p.5
  (let ((obj (allocate-instance (find-class 'slot-exists-p-class-01))))
    (slot-exists-p obj (gensym)))
  nil)

(deftest slot-exists-p.6
  (let ((obj (allocate-instance (find-class 'slot-exists-p-class-01))))
    (slot-exists-p obj nil))
  nil)

(deftest slot-exists-p.7
  (let ((obj (allocate-instance (find-class 'slot-exists-p-class-01))))
    (slot-exists-p obj t))
  nil)

;;; SLOT-EXISTS-P may be called on any object, not just on standard objects

(deftest slot-exists-p.8
  (let ((slot-name (gensym)))
    (loop for x in *universe*
	  when (slot-exists-p x slot-name)
	  collect x))
  nil)
