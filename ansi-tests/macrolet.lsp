;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Oct  9 19:41:24 2002
;;;; Contains: Tests of MACROLET

(in-package :cl-test)

(deftest macrolet.1
  (let ((z (list 3 4)))
    (macrolet ((%m (x) `(car ,x)))
      (let ((y (list 1 2)))
	(values (%m y) (%m z)))))
  1 3)

(deftest macrolet.2
  (let ((z (list 3 4)))
    (macrolet ((%m (x) `(car ,x)))
      (let ((y (list 1 2)))
	(values 
	 (setf (%m y) 6)
	 (setf (%m z) 'a)
	 y z))))
  6 a (6 2) (a 4))


;;; Inner definitions shadow outer ones
(deftest macrolet.3
  (macrolet ((%m (w) `(cadr ,w)))
    (let ((z (list 3 4)))
      (macrolet ((%m (x) `(car ,x)))
	(let ((y (list 1 2)))
	  (values 
	   (%m y) (%m z)
	   (setf (%m y) 6)
	   (setf (%m z) 'a)
	   y z)))))
  1 3 6 a (6 2) (a 4))

;;; &whole parameter
(deftest macrolet.4
  (let ((x nil))
    (macrolet ((%m (&whole w arg)
		   `(progn (setq x (quote ,w))
			   ,arg)))
      (values (%m 1) x)))
  1 (%m 1))

;;; &whole parameter (nested, destructuring; see section 3.4.4)
(deftest macrolet.5
  (let ((x nil))
    (macrolet ((%m ((&whole w arg))
		   `(progn (setq x (quote ,w))
			   ,arg)))
      (values (%m (1)) x)))
  1 (1))

;;; key parameter
(deftest macrolet.6
  (let ((x nil))
    (macrolet ((%m (&key (a 'xxx) b)
		   `(setq x (quote ,a))))
			   
      (values (%m :a foo) x
	      (%m :b bar) x)))
  foo foo xxx xxx)

;;; nested key parameters
(deftest macrolet.7
  (let ((x nil))
    (macrolet ((%m ((&key a b))
		   `(setq x (quote ,a))))
			   
      (values (%m (:a foo)) x
	      (%m (:b bar)) x)))
  foo foo nil nil)

;;; nested key parameters
(deftest macrolet.8
  (let ((x nil))
    (macrolet ((%m ((&key (a 10) b))
		   `(setq x (quote ,a))))
			   
      (values (%m (:a foo)) x
	      (%m (:b bar)) x)))
  foo foo 10 10)

;;; keyword parameter with supplied-p parameter
(deftest macrolet.9
  (let ((x nil))
    (macrolet ((%m (&key (a 'xxx a-p) b)
		   `(setq x (quote ,(list a (not (not a-p)))))))
			   
      (values (%m :a foo) x
	      (%m :b bar) x)))
  (foo t) (foo t) (xxx nil) (xxx nil))


;;; rest parameter
(deftest macrolet.10
  (let ((x nil))
    (macrolet ((%m (b &rest a)
		   `(setq x (quote ,a))))
      (values (%m a1 a2) x)))
  (a2) (a2))

;;; rest parameter w. destructuring
(deftest macrolet.11
  (let ((x nil))
    (macrolet ((%m ((b &rest a))
		   `(setq x (quote ,a))))
      (values (%m (a1 a2)) x)))
  (a2) (a2))

;;; rest parameter w. whole
(deftest macrolet.12
  (let ((x nil))
    (macrolet ((%m (&whole w b &rest a)
		   `(setq x (quote ,(list a w)))))
      (values (%m a1 a2) x)))
  ((a2) (%m a1 a2))
  ((a2) (%m a1 a2)))

;;; Interaction with symbol-macrolet

(deftest macrolet.13
  (symbol-macrolet ((a b))
    (macrolet ((foo (x &environment env)
		    (let ((y (macroexpand x env)))
		      (if (eq y 'a) 1 2))))
      (foo a)))
  2)

(deftest macrolet.14
  (symbol-macrolet ((a b))
    (macrolet ((foo (x &environment env)
		    (let ((y (macroexpand-1 x env)))
		      (if (eq y 'a) 1 2))))
      (foo a)))
  2)

(deftest macrolet.15
  (macrolet ((nil () ''a))
    (nil))
  a)

(deftest macrolet.16
   (loop for s in *cl-non-function-macro-special-operator-symbols*
	for form = `(classify-error (macrolet ((,s () ''a)) (,s)))
	unless (eq (eval form) 'a)
	collect s)
  nil)

;;; Symbol-macrolet tests

(deftest symbol-macrolet.1
  (loop for s in *cl-non-variable-constant-symbols*
	for form = `(classify-error (symbol-macrolet ((,s 17)) ,s))
	unless (eql (eval form) 17)
	collect s)
  nil)
