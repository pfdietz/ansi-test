;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar  8 22:38:53 2003
;;;; Contains: Tests of DEFINE-CONDITION (part 1)

(in-package :cl-test)

;;;

(define-condition-with-tests cond-1 nil nil)

(define-condition-with-tests cond-2 (condition) nil)

(define-condition-with-tests #:cond-3 nil nil)

(define-condition-with-tests cond-4 nil
  ((slot1 :initarg :slot1 :reader cond-4/slot-1)
   (slot2 :initarg :slot2 :reader cond-4/slot-2)))

(deftest cond-4-slots.1
  (let ((c (make-condition 'cond-4 :slot1 'a :slot2 'b)))
    (and (typep c 'cond-4)
	 (eqt (cond-4/slot-1 c) 'a)
	 (eqt (cond-4/slot-2 c) 'b)))
  t)

(define-condition-with-tests cond-5 nil
  ((slot1 :initarg :slot1 :initform 'x :reader cond-5/slot-1)
   (slot2 :initarg :slot2 :initform 'y :reader cond-5/slot-2)))

(deftest cond-5-slots.1
  (let ((c (make-condition 'cond-5 :slot1 'a :slot2 'b)))
    (and (typep c 'cond-5)
	 (eqt (cond-5/slot-1 c) 'a)
	 (eqt (cond-5/slot-2 c) 'b)))
  t)

(deftest cond-5-slots.2
  (let ((c (make-condition 'cond-5 :slot1 'a)))
    (and (typep c 'cond-5)
	 (eqt (cond-5/slot-1 c) 'a)
	 (eqt (cond-5/slot-2 c) 'y)))
  t)

(deftest cond-5-slots.3
  (let ((c (make-condition 'cond-5 :slot2 'b)))
    (and (typep c 'cond-5)
	 (eqt (cond-5/slot-1 c) 'x)
	 (eqt (cond-5/slot-2 c) 'b)))
  t)

(deftest cond-5-slots.4
  (let ((c (make-condition 'cond-5)))
    (and (typep c 'cond-5)
	 (eqt (cond-5/slot-1 c) 'x)
	 (eqt (cond-5/slot-2 c) 'y)))
  t)

(define-condition-with-tests cond-6 nil
  ((slot1 :initarg :slot1 :initarg :both-slots
	  :initform 'x :reader cond-6/slot-1)
   (slot2 :initarg :slot2 :initarg :both-slots
	  :initform 'y :reader cond-6/slot-2)))

(deftest cond-6-slots.1
  (let ((c (make-condition 'cond-6 :both-slots 'a)))
    (and (typep c 'cond-6)
	 (eqt (cond-6/slot-1 c) 'a)
	 (eqt (cond-6/slot-2 c) 'a)))
  t)

(deftest cond-6-slots.2
  (let ((c (make-condition 'cond-6)))
    (and (typep c 'cond-6)
	 (eqt (cond-6/slot-1 c) 'x)
	 (eqt (cond-6/slot-2 c) 'y)))
  t)

(deftest cond-6-slots.3
  (let ((c (make-condition 'cond-6 :slot1 'a :both-slots 'b)))
    (and (typep c 'cond-6)
	 (eqt (cond-6/slot-1 c) 'a)
	 (eqt (cond-6/slot-2 c) 'b)))
  t)

(deftest cond-6-slots.4
  (let ((c (make-condition 'cond-6 :slot2 'b :both-slots 'a)))
    (and (typep c 'cond-6)
	 (eqt (cond-6/slot-1 c) 'a)
	 (eqt (cond-6/slot-2 c) 'b)))
  t)

(deftest cond-6-slots.5
  (let ((c (make-condition 'cond-6 :both-slots 'a :slot1 'c :slot2 'd)))
    (and (typep c 'cond-6)
	 (eqt (cond-6/slot-1 c) 'a)
	 (eqt (cond-6/slot-2 c) 'a)))
  t)



