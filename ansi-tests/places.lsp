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

(deftest pushnew-order.2
  (let ((x (vector nil nil nil nil nil))
	(y (vector 'a 'b 'c 'd 'e))
	(i 1))
    (pushnew (aref y (incf i)) (aref x (incf i))
	     :test (progn (incf i) #'eql))
    (values x y i))
  #(nil nil nil (c) nil)
  #(a b c d e)
  4)

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
  3 2 1)

(deftest setf-values.3
  (let ((x nil) (y nil) (z nil))
    (setf (values x x x) (values 1 2 3))
    x)
  3)

;;; Test that the subplaces of a VALUES place can be
;;; complex, and that the various places' subforms are
;;; evaluated in the correct (left-to-right) order.

(deftest setf-values.4
  (let ((x (list 'a 'b)))
    (setf (values (car x) (cadr x)) (values 1 2))
    x)
  (1 2))

(deftest setf-values.5
  (let ((a (vector nil nil))
	(i 0)
	x y z)
    (setf (values (aref a (progn (setf x (incf i)) 0))
		  (aref a (progn (setf y (incf i)) 1)))
	  (progn
	    (setf z (incf i))
	    (values 'foo 'bar)))
    (values a i x y z))
  #(foo bar) 3 1 2 3)

(deftest setf-values.6
  (setf (values) (values)))

;;; Section 5.1.2.4
(deftest setf-the.1
  (let ((x 1))
    (setf (the integer x) 2)
    x)
  2)

(deftest setf-the.2
  (let ((x (list 'a)))
    (values
     (setf (the symbol (car x)) 'b)
     x))
  b (b))

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

;;; Tests that, being treated like SETF, this causes multiple values
;;; to be assigned to (values y z)
(deftest setf-symbol-macro.3
  (symbol-macrolet ((x (values y z)))
    (let ((y nil) (z nil))
      (values (setq x (values 1 2)) x y z)))
  1 1 1 2)

(deftest setq.1
  (setq)
  nil)

(deftest setq.2
  (let ((x 0) (y 0))
    (values (setq x 1 y 2) x y))
  2 1 2)

(deftest setq.3
  (let ((x 0) (y 0))
    (values (setq x (values 1 3) y (values 2 4)) x y))
  2 1 2)

(deftest setq.4
  (let (x) (setq x (values 1 2)))
  1)

(deftest setf.1
  (setf)
  nil)

(deftest setf.2
  (let ((x 0) (y 0))
    (values (setf x 1 y 2) x y))
  2 1 2)

(deftest setf.3
  (let ((x 0) (y 0))
    (values (setf x (values 1 3) y (values 2 4)) x y))
  2 1 2)

(deftest setf.4
  (let (x) (setf x (values 1 2)))
  1)

;;; Tests of PSETQ

(deftest psetq.1
  (psetq)
  nil)

(deftest psetq.2
  (let ((x 0))
    (values (psetq x 1) x))
  nil 1)

(deftest psetq.3
  (let ((x 0) (y 1))
    (values (psetq x y y x) x y))
  nil 1 0)

(deftest psetq.4
  (let ((x 0))
    (values
     (symbol-macrolet ((x y))
       (let ((y 1))
	 (psetq x 2)
	 y))
     x))
  2 0)

(deftest psetq.5
  (let ((w (list nil)))
    (values
     (symbol-macrolet ((x (car w)))
       (psetq x 2))
     w))
  nil (2))

(deftest psetq.6
  (let ((c 0) x y)
    (psetq x (incf c)
	   y (incf c))
    (values c x y))
  2 1 2)

;;; The next test is a PSETQ that is equivalent to a PSETF
;;; See PSETF.7 for comments related to this test.

(deftest psetq.7
  (symbol-macrolet ((x (aref a (incf i)))
		    (y (aref a (incf i))))
    (let ((a (copy-seq #(0 1 2 3 4 5 6 7 8 9)))
	  (i 0))
      (psetq x (aref a (incf i))
	     y (aref a (incf i)))
      (values a i)))
  #(0 2 2 4 4 5 6 7 8 9)
  4)

;;; Tests of PSETF

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
