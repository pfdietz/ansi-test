;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Oct  7 19:20:17 2002
;;;; Contains: Tests of various kinds of places (section 5.1)

(in-package :cl-test)

;;; Section 5.1.1.1
(deftest setf-order
  (let ((x (vector nil nil nil nil))
	(i 0))
    (setf (aref x (incf i)) (incf i))
    (values x i))
  #(nil 2 nil nil) 2)

(deftest setf-order.2
  (let ((x (vector nil nil nil nil))
	(i 0))
    (setf (aref x (incf i)) (incf i)
	  (aref x (incf i)) (incf i 10))
    (values x i))
  #(nil 2 nil 13) 13)

(deftest push-order
  (let ((x (vector nil nil nil nil))
	(y (vector 'a 'b 'c 'd))
	(i 1))
    (push (aref y (incf i)) (aref x (incf i)))
    (values x y i))
  #(nil nil nil (c))
  #(a b c d)
  3)

(deftest pushnew-order
  (let ((x (vector nil nil nil nil))
	(y (vector 'a 'b 'c 'd))
	(i 1))
    (pushnew (aref y (incf i)) (aref x (incf i)))
    (values x y i))
  #(nil nil nil (c))
  #(a b c d)
  3)

(deftest remf-order
  (let ((x  (copy-seq #(nil :a :b)))
	(pa (vector (list :a 1) (list :b 2) (list :c 3) (list :d 4)))
	(i 0))
    (values
     (not (remf (aref pa (incf i)) (aref x (incf i))))
     pa))
  nil #((:a 1) nil (:c 3) (:d 4)))

(deftest incf-order
  (let ((x (copy-seq #(0 0 0 0 0)))
	(i 1))
    (values
     (incf (aref x (incf i)) (incf i))
     x i))
  3 #(0 0 3 0 0) 3)

(deftest decf-order
  (let ((x (copy-seq #(0 0 0 0 0)))
	(i 1))
    (values
     (decf (aref x (incf i)) (incf i))
     x i))
  -3 #(0 0 -3 0 0) 3)

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

(deftest rotatef-order.1
  (let ((x (vector 'a 'b 'c 'd 'e 'f))
	(i 2))
    (values
     (rotatef (aref x (incf i)) (aref x (incf i)))
     x i))
  nil
  #(a b c e d f)
  4)

(deftest rotatef-order.2
  (let ((x (vector 'a 'b 'c 'd 'e 'f))
	(i 2))
    (values
     (rotatef (aref x (incf i)) (aref x (incf i)) (aref x (incf i)))
     x i))
  nil
  #(a b c e f d)
  5)
    
(deftest psetf-order
  (let ((x (vector nil nil nil nil))
	(i 0))
    (psetf (aref x (incf i)) (incf i))
    (values x i))
  #(nil 2 nil nil) 2)

(deftest psetf-order.2
  (let ((x (vector nil nil nil nil))
	(i 0))
    (psetf (aref x (incf i)) (incf i)
	   (aref x (incf i)) (incf i 10))
    (values x i))
  #(nil 2 nil 13) 13)

(deftest pop-order
  (let ((x (vector '(a b) '(c d) '(e f)))
	(i 0))
    (values (pop (aref x (incf i))) x i))
  c #((a b) (d) (e f)) 1)


;;; Section 5.1.2.1
(deftest setf-var
  (let ((x nil))
    (setf x 'a)
    x)
  a)

;;; Section 5.1.2.2
;;; See SETF forms at various accessor functions

;;; Section 5.1.2.3
(deftest setf-values.1
  (let ((x nil) (y nil) (z nil))
    (setf (values x y z) (values 1 2 3)))
  1 2 3)

(deftest setf-values.2
  (let ((x nil) (y nil) (z nil))
    (setf (values x y z) (values 1 2 3))
    (values z y x))
  z y x)

(deftest setf-values.3
  (let ((x nil) (y nil) (z nil))
    (setf (values x x x) (values 1 2 3))
    x)
  3)

;;; Section 5.1.2.4
(deftest setf-the.1
  (let ((x 1))
    (setf (the integer x) 2)
    x)
  2)

;;; Section 5.1.2.5
(deftest setf-apply.1
  (let ((x (vector 0 1 2 3 4 5)))
    (setf (apply #'aref x '(0)) 10)
    x)
  #(10 1 2 3 4 5))

(deftest setf-apply.2
  (let ((a (make-array '(2 2) :initial-contents '((0 0)(0 0)))))
    (setf (apply #'aref a 1 1 nil) 'a)
    (equalp a (make-array '(2 2) :initial-contents '((0 0)(0 a)))))
  t)

(deftest setf-apply.3
  (let ((bv (copy-seq #*0000000000)))
    (setf (apply #'bit bv 4 nil) 1)
    bv)
  #*0000100000)

(deftest setf-apply.4
  (let ((bv (copy-seq #*0000000000)))
    (setf (apply #'sbit bv 4 nil) 1)
    bv)
  #*0000100000)

;;; Section 5.1.2.6
(defun accessor-5-1-2-6-update-fn (x y)
  (setf (car x) y)
  y)

(defsetf accessor-5-1-2-6 accessor-5-1-2-6-update-fn)

(deftest setf-expander.1
  (let ((x (list 1)))
    (values (setf (accessor-5-1-2-6 x) 2)
	    (1+ (car x))))
  2 3)

;;; Section 5.1.2.7

(defmacro accessor-5-1-2-7 (x) `(car ,x))
(deftest setf-macro.1
  (let ((x (list 1)))
    (values (setf (accessor-5-1-2-7 x) 2)
	    (1+ (car x))))
  2 3)

(defun accessor-5-1-2-7a-update-fn (x y)
  (declare (special *x*))
  (setf (car x) y)
  (setf *x* 'boo)
  y)

(defmacro accessor-5-1-2-7a (x) `(car ,x))
(defsetf accessor-5-1-2-7a accessor-5-1-2-7a-update-fn)
;; Test that the defsetf override the macro expansion
(deftest setf-macro.2
  (let ((x (list 1))
	(*x* nil))
     (declare (special *x*))
    (values (setf (accessor-5-1-2-7a x) 2)
	    *x*
	    (1+ (car x))))
  2 boo 3)

(defmacro accessor-5-1-2-7b (x) `(accessor-5-1-2-7 ,x))
;; Test that the macroexpansion occurs more than once
(deftest setf-macro.3
  (let ((x (list 1)))
    (values (setf (accessor-5-1-2-7b x) 2)
	    (1+ (car x))))
  2 3)

;; Macroexpansion from a macrolet
(deftest setf-macro.4
  (macrolet ((%m (y) `(car ,y)))
    (let ((x (list 1)))
      (values (setf (%m x) 2)
	      (1+ (car x)))))
  2 3)

;;; section 5.1.2.8 -- symbol macros
(deftest setf-symbol-macro.1
  (symbol-macrolet ((x y))
    (let ((y nil))
      (values (setf x 1) x y)))
  1 1 1)

;;; Symbol macros in SETQs are treated as if the form were a SETF
(deftest setf-symbol-macro.2
  (symbol-macrolet ((x y))
    (let ((y nil))
      (values (setq x 1) x y)))
  1 1 1)
