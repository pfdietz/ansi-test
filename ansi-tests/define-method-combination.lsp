;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jun 15 10:49:39 2003
;;;; Contains: Tests of DEFINE-METHOD-COMBINATION

(in-package :cl-test)

(define-method-combination times
  :documentation "Multiplicative method combination, version 1"
  :operator *)

(defgeneric dmc-gf-01 (x) (:method-combination times))

(defmethod dmc-gf-01 times ((x integer)) 2)
(defmethod dmc-gf-01 times ((x rational)) 3)
(defmethod dmc-gf-01 times ((x real)) 5)
(defmethod dmc-gf-01 times ((x number)) 7)
(defmethod dmc-gf-01 times ((x complex)) 11)

(deftest define-method-combination-01.1
  (values
   (dmc-gf-01 1)
   (dmc-gf-01 1/2)
   (dmc-gf-01 1.0)
   (dmc-gf-01 #c(1 2)))
  210 105 35 77)

(deftest define-method-combination-01.2
  (handler-case
   (eval '(locally (declare (optimize (safety 3)))
		   (dmc-gf-01 'x)))
   (error () :good))
  :good)

(defgeneric dmc-gf-02 (x) (:method-combination times))

(defmethod dmc-gf-02 times ((x integer)) 2)
(defmethod dmc-gf-02 :around ((x rational)) (1- (call-next-method)))
(defmethod dmc-gf-02 times ((x real)) 3)
(defmethod dmc-gf-02 times ((x number)) 5)
(defmethod dmc-gf-02 :around ((x (eql 1.0s0))) 1)

(deftest define-method-combination-02.1
  (values
   (dmc-gf-02 1)
   (dmc-gf-02 1/3)
   (dmc-gf-02 1.0s0)
   (dmc-gf-02 13.0)
   (dmc-gf-02 #c(1 2)))
  29 14 1 15 5)

(defgeneric dmc-gf-03 (x) (:method-combination times))

(deftest define-method-combination-03.1
  (handler-case
   (progn
     (eval '(defmethod dmc-gf-03 ((x integer)) t))
     :bad)
   (error () :good))
  :good)

(deftest define-method-combination-03.2
  (handler-case
   (progn
     (eval '(defmethod dmc-gf-03 :before ((x cons)) t))
     :bad)
   (error () :good))
  :good)

(deftest define-method-combination-03.3
  (handler-case
   (progn
     (eval '(defmethod dmc-gf-03 :after ((x symbol)) t))
     :bad)
   (error () :good))
  :good)









  

