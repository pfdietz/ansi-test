;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar  8 22:38:53 2003
;;;; Contains: Tests of DEFINE-CONDITION (part 1)

(in-package :cl-test)

(define-condition-with-tests cond-1 nil nil)

(deftest cond-1.1
  (let ((c (make-condition 'cond-1)))
    (handler-case (signal c)
		  (cond-1 (c1) (eqt c c1))))
  t)

(deftest cond-1.2
  (let ((c (make-condition 'cond-1)))
    (handler-case (signal c)
		  (condition (c1) (eqt c c1))))
  t)

(deftest cond-1.3
  (let ((c (make-condition 'cond-1)))
    (handler-case (signal c)
		  (error () nil)
		  (cond-1 () t)))
  t)

(deftest cond-1.4
  (let ((c (make-condition 'cond-1)))
    (and (typep c 'cond-1)
	 (notnot-mv (typep c (find-class 'cond-1)))))
  t)



     
