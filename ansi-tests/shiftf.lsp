;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 15:43:44 2003
;;;; Contains: Tests of SHIFTF

(in-package :cl-test)

(deftest shiftf-order.1
  (let ((x (vector 'a 'b 'c 'd 'e))
	(i 2))
    (values (shiftf (aref x (incf i)) (incf i)) x i))
  d #(a b c 4 e) 4)
    
(deftest shiftf-order.2
  (let ((x (vector 'a 'b 'c 'd 'e 'f 'g 'h))
	(i 2))
    (values (shiftf (aref x (incf i)) (aref x (incf i)) (incf i)) x i))
  d #(a b c e 5 f g h) 5)

;;; Need to add more shiftf tests here
