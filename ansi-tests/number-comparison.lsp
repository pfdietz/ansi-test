;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Apr  7 07:17:42 2003
;;;; Contains: Tests of =, /=, <, <=, >, >=

(in-package :cl-test)

;;; Errors tests on comparison functions

(deftest =.error.1
  (classify-error (=))
  program-error)

(deftest /=.error.1
  (classify-error (/=))
  program-error)

(deftest <.error.1
  (classify-error (<))
  program-error)

(deftest <=.error.1
  (classify-error (<=))
  program-error)

(deftest >.error.1
  (classify-error (>))
  program-error)

(deftest >=.error.1
  (classify-error (>=))
  program-error)

;;; Tests of =

(deftest =.1
  (loop for x in *numbers*
	unless (= x)
	collect x)
  nil)

(deftest =.2
  (loop for x in *numbers*
	unless (= x x)
	collect x)
  nil)

(deftest =.3
  (loop for x in *numbers*
	unless (= x x x)
	collect x)
  nil)

(deftest =.4
  (=.4-fn)
  nil)

(deftest =.5
  (loop for i from 1 to 10000
	for i2 = (1+ i)
	never (or (= i i2) (= i2 i)))
  t)

(deftest =.6
  (loop for i from 5 to 10000 by 17
	for j from 2 to i by 19
	for r = (/ i j)
	unless (and (not (= r (1+ r)))
		    (not (= r 0))
		    (not (= r (- r)))
		    (= r r))
	collect r)
  nil)
			 
(deftest =.7
  (let ((args nil))
    (loop for i from 1 to (min 256 call-arguments-limit)
	  do (push 17 args)
	  always (apply #'= args)))
  t)

(deftest =.8
  (loop for i from 2 to (min 256 call-arguments-limit)
	for args = (append (make-list (1- i) :initial-element 7)
			   (list 23))
	when (apply #'= args)
	collect args)
  nil)

(deftest =.9
  (=t 0 0.0)
  t)

(deftest =.10
  (=t 0 #c(0 0))
  t)

(deftest =.11
  (=t 1 #c(1.0 0.0))
  t)

(deftest =.12
  (=t -0.0 0.0)
  t)
