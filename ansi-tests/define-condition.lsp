;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar  8 22:38:53 2003
;;;; Contains: Tests of DEFINE-CONDITION (part 1)

(in-package :cl-test)

;;;

(define-condition-with-tests cond-1 nil nil)

(define-condition-with-tests cond-2 (condition) nil)

#-gcl (define-condition-with-tests #:cond-3 nil nil)

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

(define-condition-with-tests cond-7 nil
  ((s :initarg :i1 :initarg :i2 :reader cond-7/s)))

(deftest cond-7-slots.1
  (let ((c (make-condition 'cond-7 :i1 'a)))
    (and (typep c 'cond-7)
	 (eqt (cond-7/s c) 'a)))
  t)

(deftest cond-7-slots.2
  (let ((c (make-condition 'cond-7 :i2 'a)))
    (and (typep c 'cond-7)
	 (eqt (cond-7/s c) 'a)))
  t)

(deftest cond-7-slots.3
  (let ((c (make-condition 'cond-7 :i1 'a :i2 'b)))
    (and (typep c 'cond-7)
	 (eqt (cond-7/s c) 'a)))
  t)

(deftest cond-7-slots.4
  (let ((c (make-condition 'cond-7 :i2 'a :i1 'b)))
    (and (typep c 'cond-7)
	 (eqt (cond-7/s c) 'a)))
  t)

(defparameter *cond-8-counter* 0)

(define-condition-with-tests cond-8 nil
  ((s :initarg :i1 :initform (incf *cond-8-counter*) :reader cond-8/s)))

(deftest cond-8-slots.1
  (let ((*cond-8-counter* 100))
    (declare (special *cond-8-counter*))
    (values
     (cond-8/s (make-condition 'cond-8))
     *cond-8-counter*))
  101 101)

(define-condition-with-tests cond-9 nil
  ((s1 :initarg :i1 :initform 15 :reader cond-9/s1)
   (s2 :initarg :i2 :initform 37 :reader cond-9/s2)))

(deftest cond-9-slots.1
  (let ((c (make-condition 'cond-9)))
    (values (notnot (typep c 'cond-9))
	    (cond-9/s1 c)
	    (cond-9/s2 c)))
  t 15 37)

(deftest cond-9-slots.2
  (let ((c (make-condition 'cond-9 :i1 3)))
    (values (notnot (typep c 'cond-9))
	    (cond-9/s1 c)
	    (cond-9/s2 c)))
  t 3 37)

(deftest cond-9-slots.3
  (let ((c (make-condition 'cond-9 :i2 3)))
    (values (notnot (typep c 'cond-9))
	    (cond-9/s1 c)
	    (cond-9/s2 c)))
  t 15 3)

(deftest cond-9-slots.4
  (let ((c (make-condition 'cond-9 :i2 3 :i2 8)))
    (values (notnot (typep c 'cond-9))
	    (cond-9/s1 c)
	    (cond-9/s2 c)))
  t 15 3)

(deftest cond-9-slots.5
  (let ((c (make-condition 'cond-9 :i1 3 :i2 8)))
    (values (notnot (typep c 'cond-9))
	    (cond-9/s1 c)
	    (cond-9/s2 c)))
  t 3 8)

(deftest cond-9-slots.6
  (let ((c (make-condition 'cond-9 :i1 3 :i2 8 :i1 100 :i2 500)))
    (values (notnot (typep c 'cond-9))
	    (cond-9/s1 c)
	    (cond-9/s2 c)))
  t 3 8)

;;; (define-condition-with-tests cond-10 nil
;;;   ((s1 :initarg :i1 :writer cond-10/s1-w :reader cond-10/s1-r)))
;;; 
;;; (deftest cond-10-slots.1
;;;   (let ((c (make-condition 'cond-10 :i1 11)))
;;;      (cond-10/s1-r c))
;;;   11)
;;; 
;;; (deftest cond-10-slots.2
;;;   (let ((c (make-condition 'cond-10 :i1 11)))
;;;      (cond-10/s1-w 17 c))
;;;   17)
;;; 
;;; (deftest cond-10-slots.3
;;;   (let ((c (make-condition 'cond-10 :i1 11)))
;;;      (cond-10/s1-w 107 c)
;;;      (cond-10/s1-r c))
;;;   107)
;;; 
;;; (define-condition-with-tests cond-11 nil
;;;   ((s1 :initarg :i1 :writer (setf cond-11/w) :reader cond-11/r)))
;;; 
;;; (deftest cond-11-slots.1
;;;   (let ((c (make-condition 'cond-11 :i1 11)))
;;;      (cond-11/r c))
;;;   11)
;;; 
;;; (deftest cond-11-slots.2
;;;   (let ((c (make-condition 'cond-11 :i1 11)))
;;;      (setf (cond-11/w c) 17))
;;;   17)
;;; 
;;; (deftest cond-11-slots.3
;;;   (let ((c (make-condition 'cond-11 :i1 11)))
;;;      (setf (cond-11/w c) 117)
;;;      (cond-11/r c))
;;;   117)
;;; 
;;; (deftest cond-11-slots.4
;;;   (let ((c (make-condition 'cond-11 :i1 11)))
;;;     (values
;;;      (funcall #'(setf cond-11/w) 117 c)
;;;      (cond-11/r c)))
;;;   117 117)

(define-condition-with-tests cond-12 nil
  (((slot1) :initarg :slot1 :reader cond-12/slot-1)
   ((slot2) :initarg :slot2 :reader cond-12/slot-2)))

(deftest cond-12-slots.1
  (let ((c (make-condition 'cond-12 :slot1 'a :slot2 'b)))
    (and (typep c 'cond-12)
	 (eqt (cond-12/slot-1 c) 'a)
	 (eqt (cond-12/slot-2 c) 'b)))
  t)

(define-condition-with-tests cond-13 nil
  (((slot1 10) :initarg :slot1 :reader cond-13/slot-1)))

(deftest cond-13-slots.1
  (let ((c (make-condition 'cond-13)))
    (and (typep c 'cond-13)
	 (cond-13/slot-1 c)))
  10)

(define-condition-with-tests cond-14 nil
  ((s1 :initarg :i1 :type fixnum :reader cond-14/s1)
   (s2 :initarg :i2 :type t :reader cond-14/s2)))

(deftest cond-14-slots.1
  (let ((c (make-condition 'cond-14 :i1 10)))
    (and (typep c 'cond-14)
	 (cond-14/s1 c)))
  10)

(deftest cond-14-slots.2
  (let ((c (make-condition 'cond-14 :i2 'a)))
    (and (typep c 'cond-14)
	 (cond-14/s2 c)))
  a)

(deftest cond-14-slots.3
  (let ((c (make-condition 'cond-14 :i1 10 :i2 'h)))
    (and (typep c 'cond-14)
	 (eqt (cond-14/s1 c) 10)
	 (cond-14/s2 c)))
  h)

(define-condition-with-tests cond-15 nil
  ((s1 :type nil)))

(define-condition-with-tests cond-16 nil
  ((slot1))
  (:report "The report for cond-16"))

(deftest cond-16-report.1
  (let ((*print-escape* nil)
	(c (make-condition 'cond-16)))
    (with-output-to-string (s) (print-object c s)))
  "The report for cond-16")

(define-condition-with-tests cond-17 nil
  ((s :initarg :i1 :reader cond-17/s ))
  (:report cond-17-report))

(defun cond-17-report (c s)
  (format s "cond-17: ~A" (cond-17/s c)))

(deftest cond-17-report.1
  (let ((*print-escape* nil)
	(c (make-condition 'cond-17 :i1 1234)))
    (with-output-to-string (s) (print-object c s)))
  "cond-17: 1234")

(define-condition-with-tests cond-18 nil
  ((s :initarg :i1 :reader cond-18/s ))
  (:report (lambda (c s) (format s "cond-18: ~A" (cond-18/s c)))))

(deftest cond-18-report.1
  (let ((*print-escape* nil)
	(c (make-condition 'cond-18 :i1 4321)))
    (with-output-to-string (s) (print-object c s)))
  "cond-18: 4321")
