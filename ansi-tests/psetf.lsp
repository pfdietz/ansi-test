;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 15:38:30 2003
;;;; Contains: Tests of PSETF

(in-package :cl-test)

(deftest psetf.order.1
  (let ((x (vector nil nil nil nil))
	(i 0))
    (psetf (aref x (incf i)) (incf i))
    (values x i))
  #(nil 2 nil nil) 2)

(deftest psetf.order.2
  (let ((x (vector nil nil nil nil))
	(i 0))
    (psetf (aref x (incf i)) (incf i)
	   (aref x (incf i)) (incf i 10))
    (values x i))
  #(nil 2 nil 13) 13)

(deftest psetf.1
  (psetf)
  nil)

(deftest psetf.2
  (let ((x 0))
    (values (psetf x 1) x))
  nil 1)

(deftest psetf.3
  (let ((x 0) (y 1))
    (values (psetf x y y x) x y))
  nil 1 0)

(deftest psetf.4
  (let ((x 0))
    (values
     (symbol-macrolet ((x y))
       (let ((y 1))
	 (psetf x 2)
	 y))
     x))
  2 0)

(deftest psetf.5
  (let ((w (list nil)))
    (values
     (symbol-macrolet ((x (car w)))
       (psetf x 2))
     w))
  nil (2))

(deftest psetf.6
  (let ((c 0) x y)
    (psetf x (incf c)
	   y (incf c))
    (values c x y))
  2 1 2)

;;; According to the standard, the forms to be assigned and
;;; the subforms in the places to be assigned to are evaluated
;;; from left to right.  Therefore, PSETF.7 and PSETF.8 should
;;; do the same thing to A as PSETF.9 does.
;;; (See the page for PSETF)

(deftest psetf.7
  (symbol-macrolet ((x (aref a (incf i)))
		    (y (aref a (incf i))))
    (let ((a (copy-seq #(0 1 2 3 4 5 6 7 8 9)))
	  (i 0))
      (psetf x (aref a (incf i))
	     y (aref a (incf i)))
      (values a i)))
  #(0 2 2 4 4 5 6 7 8 9)
  4)

(deftest psetf.8
  (let ((a (copy-seq #(0 1 2 3 4 5 6 7 8 9)))
	(i 0))
    (psetf (aref a (incf i)) (aref a (incf i))
	   (aref a (incf i)) (aref a (incf i)))
    (values a i))
  #(0 2 2 4 4 5 6 7 8 9)
  4)

(deftest psetf.9
  (let ((a (copy-seq #(0 1 2 3 4 5 6 7 8 9))))
    (psetf (aref a 1) (aref a 2)
	   (aref a 3) (aref a 4))
    a)
  #(0 2 2 4 4 5 6 7 8 9))

(deftest psetf.10
  (let ((*x* 0) (*y* 10))
    (declare (special *x* *y*))
    (values
     *x* *y*
     (psetf *x* 6
	    *y* 15)
     *x* *y*))
  0 10 nil 6 15)

(deftest psetf.11
  (let ((*x* 0) (*y* 10))
    (declare (special *x* *y*))
    (values
     *x* *y*
     (psetf *x* *y*
	    *y* *x*)
     *x* *y*))
  0 10 nil 10 0)

(def-macro-test psetf.error.1 (psetf))
