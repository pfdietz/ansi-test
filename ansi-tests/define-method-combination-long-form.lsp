;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jul 13 08:26:41 2003
;;;; Contains: Tests of DEFINE-METHOD-COMBINATION (long form)

(in-package :cl-test)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (report-and-ignore-errors
   (defparameter *dmc-long-01*
     (define-method-combination mc-long-01 nil nil)))
  (report-and-ignore-errors
   (defgeneric dmc-long-gf-01 (x y) (:method-combination mc-long-01)))
  )

(deftest define-method-combination-long.01.1
  (eqt *dmc-long-01* 'mc-long-01)
  t)

;;; The list of method groups specifiers for this method combination
;;; is empty, so no methods are valid.
(deftest define-method-combination-long.01.2
  (progn
    (eval '(defmethod dmc-long-gf-01 ((x t) (y t)) :foo))
    (handler-case
     (dmc-long-gf-01 'a 'b)
     (error () :caught)))
  :caught)

   

     