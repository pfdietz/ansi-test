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

;;; The cond-12 and cond-13 tests have been removed.  Duane Rettig
;;; convincingly argued that the feature being tested (non-symbol
;;; slot names) remains in the standard only because of editing errors.

;;; (define-condition-with-tests cond-12 nil
;;;   (((slot1) :initarg :slot1 :reader cond-12/slot-1)
;;;    ((slot2) :initarg :slot2 :reader cond-12/slot-2)))
;;; 
;;; (deftest cond-12-slots.1
;;;   (let ((c (make-condition 'cond-12 :slot1 'a :slot2 'b)))
;;;     (and (typep c 'cond-12)
;;; 	 (eqt (cond-12/slot-1 c) 'a)
;;; 	 (eqt (cond-12/slot-2 c) 'b)))
;;;   t)
;;; 
;;; (define-condition-with-tests cond-13 nil
;;;   (((slot1 10) :initarg :slot1 :reader cond-13/slot-1)))
;;; 
;;; (deftest cond-13-slots.1
;;;   (let ((c (make-condition 'cond-13)))
;;;     (and (typep c 'cond-13)
;;; 	 (cond-13/slot-1 c)))
;;;   10)
 
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

;;;
;;; Tests of :default-initargs
;;;
;;; There is an inconsistency in the ANSI spec.  DEFINE-CONDITION
;;; says that in (:default-initargs . <foo>), <foo> is a list of pairs.
;;; However, DEFCLASS says it's a list whose alternate elements
;;; are initargs and initforms.  I have taken the second interpretation.
;;;

(define-condition-with-tests cond-19 nil
  ((s1 :reader cond-19/s1 :initarg :i1)
   (s2 :reader cond-19/s2 :initarg :i2))
  (:default-initargs :i1 10
		     :i2 20))

(deftest cond-19-slots.1
  (let ((c (make-condition 'cond-19)))
    (values
     (notnot (typep c 'cond-19))
     (cond-19/s1 c)
     (cond-19/s2 c)))
  t 10 20)

(deftest cond-19-slots.2
  (let ((c (make-condition 'cond-19 :i1 'a)))
    (values
     (notnot (typep c 'cond-19))
     (cond-19/s1 c)
     (cond-19/s2 c)))
  t a 20)

(deftest cond-19-slots.3
  (let ((c (make-condition 'cond-19 :i2 'a)))
    (values
     (notnot (typep c 'cond-19))
     (cond-19/s1 c)
     (cond-19/s2 c)))
  t 10 a)

(deftest cond-19-slots.4
  (let ((c (make-condition 'cond-19 :i1 'x :i2 'y)))
    (values
     (notnot (typep c 'cond-19))
     (cond-19/s1 c)
     (cond-19/s2 c)))
  t x y)

(deftest cond-19-slots.5
  (let ((c (make-condition 'cond-19 :i2 'y :i1 'x)))
    (values
     (notnot (typep c 'cond-19))
     (cond-19/s1 c)
     (cond-19/s2 c)))
  t x y)

(defparameter *cond-20/s1-val* 0)
(defparameter *cond-20/s2-val* 0)

(define-condition-with-tests cond-20 nil
  ((s1 :reader cond-20/s1 :initarg :i1)
   (s2 :reader cond-20/s2 :initarg :i2))
  (:default-initargs :i1 (incf *cond-20/s1-val*)
		     :i2 (incf *cond-20/s2-val*)))

(deftest cond-20-slots.1
  (let ((*cond-20/s1-val* 0)
	(*cond-20/s2-val* 10))
    (declare (special *cond-20/s1-val* *cond-20/s2-val*))
    (let ((c (make-condition 'cond-20)))
      (values
       (notnot (typep c 'cond-20))
       (cond-20/s1 c)
       (cond-20/s2 c)
       *cond-20/s1-val*
       *cond-20/s2-val*)))
  t 1 11 1 11)

(deftest cond-20-slots.2
  (let ((*cond-20/s1-val* 0)
	(*cond-20/s2-val* 10))
    (declare (special *cond-20/s1-val* *cond-20/s2-val*))
    (let ((c (make-condition 'cond-20 :i1 'x)))
      (values
       (notnot (typep c 'cond-20))
       (cond-20/s1 c)
       (cond-20/s2 c)
       *cond-20/s1-val*
       *cond-20/s2-val*)))
  t x 11 0 11)

(deftest cond-20-slots.3
  (let ((*cond-20/s1-val* 0)
	(*cond-20/s2-val* 10))
    (declare (special *cond-20/s1-val* *cond-20/s2-val*))
    (let ((c (make-condition 'cond-20 :i2 'y)))
      (values
       (notnot (typep c 'cond-20))
       (cond-20/s1 c)
       (cond-20/s2 c)
       *cond-20/s1-val*
       *cond-20/s2-val*)))
  t 1 y 1 10)

(deftest cond-20-slots.4
  (let ((*cond-20/s1-val* 0)
	(*cond-20/s2-val* 10))
    (declare (special *cond-20/s1-val* *cond-20/s2-val*))
    (let ((c (make-condition 'cond-20 :i2 'y :i1 'x)))
      (values
       (notnot (typep c 'cond-20))
       (cond-20/s1 c)
       (cond-20/s2 c)
       *cond-20/s1-val*
       *cond-20/s2-val*)))
  t x y 0 10)


;;;;;;;;; tests of inheritance

(define-condition-with-tests cond-21 (cond-4) nil)

(deftest cond-21-slots.1
  (let ((c (make-condition 'cond-21 :slot1 'a :slot2 'b)))
    (and (typep c 'cond-4)
	 (typep c 'cond-21)
	 (eqt (cond-4/slot-1 c) 'a)
	 (eqt (cond-4/slot-2 c) 'b)))
  t)

(define-condition-with-tests cond-22 (cond-4)
  ((slot3 :initarg :slot3 :reader cond-22/slot-3)
   (slot4 :initarg :slot4 :reader cond-22/slot-4)))

(deftest cond-22-slots.1
  (let ((c (make-condition 'cond-22 :slot1 'a :slot2 'b
			   :slot3 'c :slot4 'd)))
    (and (typep c 'cond-4)
	 (typep c 'cond-22)
	 (eqt (cond-4/slot-1 c) 'a)
	 (eqt (cond-4/slot-2 c) 'b)
	 (eqt (cond-22/slot-3 c) 'c)
	 (eqt (cond-22/slot-4 c) 'd)
	 ))
  t)

(define-condition-with-tests cond-23 (cond-5) nil)

(deftest cond-23-slots.1
  (let ((c (make-condition 'cond-23 :slot1 'a :slot2 'b)))
    (and (typep c 'cond-5)
	 (typep c 'cond-23)
	 (eqt (cond-5/slot-1 c) 'a)
	 (eqt (cond-5/slot-2 c) 'b)
	 ))
  t)

(deftest cond-23-slots.2
  (let ((c (make-condition 'cond-23 :slot1 'a)))
    (and (typep c 'cond-5)
	 (typep c 'cond-23)
	 (eqt (cond-5/slot-1 c) 'a)
	 (eqt (cond-5/slot-2 c) 'y)
	 ))
  t)

(deftest cond-23-slots.3
  (let ((c (make-condition 'cond-23 :slot2 'b)))
    (and (typep c 'cond-5)
	 (typep c 'cond-23)
	 (eqt (cond-5/slot-1 c) 'x)
	 (eqt (cond-5/slot-2 c) 'b)
	 ))
  t)

(deftest cond-23-slots.4
  (let ((c (make-condition 'cond-23)))
    (and (typep c 'cond-5)
	 (typep c 'cond-23)
	 (eqt (cond-5/slot-1 c) 'x)
	 (eqt (cond-5/slot-2 c) 'y)
	 ))
  t)

(define-condition-with-tests cond-24 (cond-5)
  nil
  (:default-initargs :slot1 'z))

(deftest cond-24-slots.1
  (let ((c (make-condition 'cond-24)))
    (and (typep c 'cond-5)
	 (typep c 'cond-24)
	 (eqt (cond-5/slot-1 c) 'z)
	 (eqt (cond-5/slot-2 c) 'y)
	 ))
  t)

(deftest cond-24-slots.2
  (let ((c (make-condition 'cond-24 :slot1 'a)))
    (and (typep c 'cond-5)
	 (typep c 'cond-24)
	 (eqt (cond-5/slot-1 c) 'a)
	 (eqt (cond-5/slot-2 c) 'y)
	 ))
  t)

(deftest cond-24-slots.3
  (let ((c (make-condition 'cond-24 :slot2 'a)))
    (and (typep c 'cond-5)
	 (typep c 'cond-24)
	 (eqt (cond-5/slot-1 c) 'z)
	 (eqt (cond-5/slot-2 c) 'a)
	 ))
  t)

(deftest cond-24-slots.4
  (let ((c (make-condition 'cond-24 :slot1 'b :slot2 'a)))
    (and (typep c 'cond-5)
	 (typep c 'cond-24)
	 (eqt (cond-5/slot-1 c) 'b)
	 (eqt (cond-5/slot-2 c) 'a)
	 ))
  t)

;;; Multiple inheritance

(define-condition-with-tests cond-25a nil
  ((s1 :initarg :s1 :initform 'a :reader cond-25a/s1)))

(define-condition-with-tests cond-25b nil
  ((s2 :initarg :s2 :initform 'b :reader cond-25b/s2)))

(define-condition-with-tests cond-25 (cond-25a cond-25b)
  ((s3 :initarg :s3 :initform 'c :reader cond-25/s3)))

(deftest cond-25-slots.1
  (let ((c (make-condition 'cond-25)))
    (and (typep c 'cond-25a)
	 (typep c 'cond-25b)
	 (typep c 'cond-25)
	 (eqt (cond-25a/s1 c) 'a)
	 (eqt (cond-25b/s2 c) 'b)
	 (eqt (cond-25/s3 c) 'c)))
  t)

(deftest cond-25-slots.2
  (let ((c (make-condition 'cond-25 :s1 'x)))
    (and (typep c 'cond-25a)
	 (typep c 'cond-25b)
	 (typep c 'cond-25)
	 (eqt (cond-25a/s1 c) 'x)
	 (eqt (cond-25b/s2 c) 'b)
	 (eqt (cond-25/s3 c) 'c)))
  t)

(deftest cond-25-slots.3
  (let ((c (make-condition 'cond-25 :s2 'x)))
    (and (typep c 'cond-25a)
	 (typep c 'cond-25b)
	 (typep c 'cond-25)
	 (eqt (cond-25a/s1 c) 'a)
	 (eqt (cond-25b/s2 c) 'x)
	 (eqt (cond-25/s3 c) 'c)))
  t)

(deftest cond-25-slots.4
  (let ((c (make-condition 'cond-25 :s3 'x)))
    (and (typep c 'cond-25a)
	 (typep c 'cond-25b)
	 (typep c 'cond-25)
	 (eqt (cond-25a/s1 c) 'a)
	 (eqt (cond-25b/s2 c) 'b)
	 (eqt (cond-25/s3 c) 'x)))
  t)

(deftest cond-25-slots.5
  (let ((c (make-condition 'cond-25 :s3 'z :s2 'y :s1 'x)))
    (and (typep c 'cond-25a)
	 (typep c 'cond-25b)
	 (typep c 'cond-25)
	 (eqt (cond-25a/s1 c) 'x)
	 (eqt (cond-25b/s2 c) 'y)
	 (eqt (cond-25/s3 c) 'z)))
  t)

;;;

(define-condition-with-tests cond-26a nil
  ((s1 :initarg :s1 :initform 'a :reader cond-26a/s1)))

(define-condition-with-tests cond-26b (cond-26a) nil)
(define-condition-with-tests cond-26c (cond-26a) nil)
(define-condition-with-tests cond-26 (cond-26b cond-26c) nil)

(deftest cond-26-slots.1
  (let ((c (make-condition 'cond-26)))
    (and (typep c 'cond-26a)
	 (typep c 'cond-26b)
	 (typep c 'cond-26c)
	 (typep c 'cond-26)
	 (eqt (cond-26a/s1 c) 'a)))
  t)

(deftest cond-26-slots.2
  (let ((c (make-condition 'cond-26 :s1 'x)))
    (and (typep c 'cond-26a)
	 (typep c 'cond-26b)
	 (typep c 'cond-26c)
	 (typep c 'cond-26)
	 (eqt (cond-26a/s1 c) 'x)))
  t)


;;; Test that a slot reader is truly a generic function

(define-condition-with-tests cond-27a nil
  ((s0 :initarg :s0 :initform 10 :reader cond-27a/s0)
   (s1 :initarg :s1 :initform 'a :reader cond-27/s1)))

(define-condition-with-tests cond-27b nil
  ((s1 :initarg :s1 :initform 'a :reader cond-27/s1)
   (s2 :initarg :s2 :initform 16 :reader cond-27b/s2)))

(deftest cond-27-slots.1
  (let ((c (make-condition 'cond-27a)))
    (and (typep c 'cond-27a)
	 (not (typep c 'cond-27b))
	 (eqt (cond-27/s1 c) 'a)))
  t)

(deftest cond-27-slots.2
  (let ((c (make-condition 'cond-27b)))
    (and (typep c 'cond-27b)
	 (not (typep c 'cond-27a))
	 (eqt (cond-27/s1 c) 'a)))
  t)

(deftest cond-27-reader-is-generic
  (notnot-mv (typep #'cond-27/s1 'generic-function))
  t)

;;; More inheritance

;;; These test that condition slots are inherited like CLOS
;;; slots.  It's not entirely clear to me if the standard
;;; demands this (one of the issues does, but that issue wasn't
;;; fully integrated into the standard.)

#|
(define-condition-with-tests cond-28a nil
  ((s1 :initarg :i1 :initform 'x :reader cond-28a/s1)))

(define-condition-with-tests cond-28 (cond-28a)
  ((s1 :initarg :i1a :reader cond-28/s1)))

(deftest cond-28-slots.1
  (let ((c (make-condition 'cond-28)))
    (and (typep c 'cond-28a)
	 (typep c 'cond-28)
	 (eqt (cond-28a/s1 c) 'x)
	 (eqt (cond-28/s1 c) 'x)))
  t)

(deftest cond-28-slots.2
  (let ((c (make-condition 'cond-28 :i1 'z)))
    (and (typep c 'cond-28a)
	 (typep c 'cond-28)
	 (eqt (cond-28a/s1 c) 'z)
	 (eqt (cond-28/s1 c) 'z)))
  t)

(deftest cond-28-slots.3
  (let ((c (make-condition 'cond-28 :i1a 'w)))
    (and (typep c 'cond-28a)
	 (typep c 'cond-28)
	 (eqt (cond-28a/s1 c) 'w)
	 (eqt (cond-28/s1 c) 'w)))
  t)

(deftest cond-28-slots.4
  (let ((c (make-condition 'cond-28 :i1 'y :i1a 'w)))
    (and (typep c 'cond-28a)
	 (typep c 'cond-28)
	 (eqt (cond-28a/s1 c) 'y)
	 (eqt (cond-28/s1 c) 'y)))
  t)

(deftest cond-28-slots.5
  (let ((c (make-condition 'cond-28 :i1a 'y :i1 'w)))
    (and (typep c 'cond-28a)
	 (typep c 'cond-28)
	 (eqt (cond-28a/s1 c) 'y)
	 (eqt (cond-28/s1 c) 'y)))
  t)
|#


;;; Documentation

;;; Pitman says this should have been in the spec, but it isn't really
;;; (define-condition-with-tests cond-29 nil
;;;  ((s1 :initarg :i1 :initform 'x
;;;       :documentation "This is slot s1 in condition cond-29")))

(define-condition-with-tests cond-30 nil
  ((s1 :initarg :i1 :initform 'x))
  (:documentation "This is class cond-30"))
