;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Oct  8 22:55:02 2002
;;;; Contains: Tests of FLET

(in-package :cl-test)

(deftest flet.1
  (flet ((%f () 1))
    (%f))
  1)

(deftest flet.2
  (flet ((%f (x) x))
    (%f 2))
  2)

(deftest flet.3
  (flet ((%f (&rest args) args))
    (%f 'a 'b 'c))
  (a b c))

;;; The optional arguments are not in the block defined by
;;; the local function declaration
(deftest flet.4
  (block %f
    (flet ((%f (&optional (x (return-from %f 10)))
	       20))
      (%f)))
  10)

(deftest flet.5
  (flet ((%f () (return-from %f 15) 35))
    (%f))
  15)

;;; The aux parameters are not in the block defined by
;;; the local function declaration
(deftest flet.6
  (block %f
    (flet ((%f (&aux (x (return-from %f 10)))
	       20))
      (%f)))
  10)

;;; The function is not visible inside itself
(deftest flet.7
  (flet ((%f (x) (+ x 5)))
    (flet ((%f (y) (cond ((eql y 20) 30)
			 (t (%f 20)))))
      (%f 15)))
  25)

;;; Keyword arguments
(deftest flet.8
  (flet ((%f (&key a (b 0 b-p)) (values a b (not (not b-p)))))
    (%f))
  nil 0 nil)

(deftest flet.9
  (flet ((%f (&key a (b 0 b-p)) (values a b (not (not b-p)))))
    (%f :a 1))
  1 0 nil)

(deftest flet.10
  (flet ((%f (&key a (b 0 b-p)) (values a b (not (not b-p)))))
    (%f :b 2))
  nil 2 t)

(deftest flet.11
  (flet ((%f (&key a (b 0 b-p)) (values a b (not (not b-p)))))
    (%f :b 2 :a 3))
  3 2 t)

;;; Unknown keyword parameter should throw a program-error in safe code
;;; (section 3.5.1.4)
(deftest flet.12
  (classify-error
   (flet ((%f (&key a (b 0 b-p)) (values a b (not (not b-p))))) (%f :c 4)))
  program-error)

;;; Odd # of keyword args should throw a program-error in safe code
;;; (section 3.5.1.6)
(deftest flet.13
  (classify-error
   (flet ((%f (&key a (b 0 b-p)) (values a b (not (not b-p))))) (%f :a)))
  program-error)

;;; Too few arguments (section 3.5.1.2)
(deftest flet.14
  (classify-error (flet ((%f (a) a)) (%f)))
  program-error)

;;; Too many arguments (section 3.5.1.3)
(deftest flet.15
  (classify-error (flet ((%f (a) a)) (%f 1 2)))
  program-error)

;;; Invalid keyword argument (section 3.5.1.5)
(deftest flet.16
  (classify-error (flet ((%f (&key a) a)) (%f '(foo))))
  program-error)


;;; Definition of a (setf ...) function

(deftest flet.17
  (flet (((setf %f) (x y) (setf (car y) x)))
    (let ((z (list 1 2)))
      (setf (%f z) 'a)
      z))
  (a 2))

;;; Body is an implicit progn
(deftest flet.18
  (flet ((%f (x) (incf x) (+ x x)))
    (%f 10))
  22)

;;; Can handle at least 50 lambda parameters
(deftest flet.19
  (flet ((%f (a1 a2 a3 a4 a5 a6 a7 a8 a9 a10
	      b1 b2 b3 b4 b5 b6 b7 b8 b9 b10
	      c1 c2 c3 c4 c5 c6 c7 c8 c9 c10
	      d1 d2 d3 d4 d5 d6 d7 d8 d9 d10
	      e1 e2 e3 e4 e5 e6 e7 e8 e9 e10)
	     (+ a1 a2 a3 a4 a5 a6 a7 a8 a9 a10
		b1 b2 b3 b4 b5 b6 b7 b8 b9 b10
		c1 c2 c3 c4 c5 c6 c7 c8 c9 c10
		d1 d2 d3 d4 d5 d6 d7 d8 d9 d10
		e1 e2 e3 e4 e5 e6 e7 e8 e9 e10)))
    (%f 1 2 3 4 5 6 7 8 9 10
	11 12 13 14 15 16 17 18 19 20
	21 22 23 24 25 26 27 28 29 30
	31 32 33 34 35 36 37 38 39 40
	41 42 43 44 45 46 47 48 49 50))
  1275)

;;; flet works with a large (maximal?) number of arguments
(deftest flet.20
  (let* ((n (min lambda-parameters-limit 1024))
	 (vars (loop for i from 1 to n collect (gensym))))
    (eval
     `(eql ,n
	   (flet ((%f ,vars (+ ,@ vars)))
	     (%f ,@(loop for e in vars collect 1))))))
  t)

;;; Declarations and documentation strings are ok
(deftest flet.21
  (flet ((%f (x)
	     (declare (type fixnum x))
	     "Add one to the fixnum x."
	     (1+ x)))
    (declare (ftype (function (fixnum) integer) %f))
    (%f 10))
  11)

(deftest flet.22
  (flet ((%f (x &optional (y 1 y-p) (z 2 z-p))
	     (list x y (not (not y-p)) z (not (not z-p)))))
    (values (%f 10) (%f 20 40) (%f 'a 'b 'c)))
  (10 1 nil 2 nil)
  (20 40 t 2 nil)
  (a b t c t))

(deftest flet.23
  (flet ((%f (x &optional (y 1 y-p) (z 2 z-p) &rest r)
	     (list x y (not (not y-p)) z (not (not z-p)) r)))
    (values (%f 10) (%f 20 40) (%f 'a 'b 'c) (%f 'd 'e 'f 'g 'h)))
  (10 1 nil 2 nil nil)
  (20 40 t 2 nil nil)
  (a b t c t nil)
  (d e t f t (g h)))

(deftest flet.24
  (flet ((%f (x &optional (y 1 y-p) (z 2 z-p) &rest r &key foo bar)
	     (list x y (not (not y-p)) z (not (not z-p)) r foo bar)))
    (values (%f 10) (%f 20 40) (%f 'a 'b 'c)
	    (%f 'd 'e 'f :foo 'h)
	    (%f 'd 'e 'f :bar 'i) ))
  (10 1 nil 2 nil nil nil nil)
  (20 40 t 2 nil nil nil nil)
  (a b t c t nil nil nil)
  (d e t f t (:foo h) h nil)
  (d e t f t (:bar i) nil i))

(deftest flet.25
  (flet ((%f (x &optional (y 1 y-p) (z 2 z-p) &rest r &key foo bar
		&allow-other-keys)
	     (list x y (not (not y-p)) z (not (not z-p)) r foo bar)))
    (values (%f 10) (%f 20 40) (%f 'a 'b 'c)
	    (%f 'd 'e 'f :foo 'h :whatever nil)
	    (%f 'd 'e 'f :bar 'i :illegal t :foo 'z) ))
  (10 1 nil 2 nil nil nil nil)
  (20 40 t 2 nil nil nil nil)
  (a b t c t nil nil nil)
  (d e t f t (:foo h :whatever nil) h nil)
  (d e t f t (:bar i :illegal t :foo z) z i))

(deftest flet.26
  (flet ((%f (x &optional (y 1 y-p) (z 2 z-p) &rest r &key foo bar)
	     (list x y (not (not y-p)) z (not (not z-p)) r foo bar)))
    (values (%f 10) (%f 20 40) (%f 'a 'b 'c)
	    (%f 'd 'e 'f :foo 'h :whatever nil :allow-other-keys t)
	    (%f 'd 'e 'f :bar 'i :illegal t :foo 'z :allow-other-keys t) ))
  (10 1 nil 2 nil nil nil nil)
  (20 40 t 2 nil nil nil nil)
  (a b t c t nil nil nil)
  (d e t f t (:foo h :whatever nil :allow-other-keys t) h nil)
  (d e t f t (:bar i :illegal t :foo z :allow-other-keys t) z i))

;;; Section 3.4.1.4.1: "The :allow-other-keys argument is permissible
;;; in all situations involving keyword[2] arguments, even when its
;;; associated value is false."
(deftest flet.27
  (flet ((%f (x &optional (y 1 y-p) (z 2 z-p) &rest r &key foo bar)
	     (list x y (not (not y-p)) z (not (not z-p)) r foo bar)))
    (values (%f 10) (%f 20 40) (%f 'a 'b 'c)
	    (%f 'd 'e 'f :foo 'h :allow-other-keys nil)
	    (%f 'd 'e 'f :bar 'i :allow-other-keys nil) ))
  (10 1 nil 2 nil nil nil nil)
  (20 40 t 2 nil nil nil nil)
  (a b t c t nil nil nil)
  (d e t f t (:foo h :allow-other-keys nil) h nil)
  (d e t f t (:bar i :allow-other-keys nil) nil i))

(deftest flet.28
  (flet ((%f (x &optional (y 1 y-p) (z 2 z-p) &rest r
		&key foo bar allow-other-keys)
	     (list x y (not (not y-p)) z (not (not z-p)) allow-other-keys
		   r foo bar)))
    (values (%f 10) (%f 20 40) (%f 'a 'b 'c)
	    (%f 'd 'e 'f :foo 'h :whatever nil :allow-other-keys 100)
	    (%f 'd 'e 'f :bar 'i :illegal t :foo 'z :allow-other-keys 200) ))
  (10 1 nil 2 nil nil nil nil nil)
  (20 40 t 2 nil nil nil nil nil)
  (a b t c t nil nil nil nil)
  (d e t f t 100 (:foo h :whatever nil :allow-other-keys 100) h nil)
  (d e t f t 200 (:bar i :illegal t :foo z :allow-other-keys 200) z i))

(deftest flet.29
  (flet ((%f (x &optional (y 1 y-p) (z 2 z-p) &rest r
		&key foo bar allow-other-keys &allow-other-keys)
	     (list x y (not (not y-p)) z (not (not z-p)) allow-other-keys
		   r foo bar)))
    (values (%f 10) (%f 20 40) (%f 'a 'b 'c)
	    (%f 'd 'e 'f :foo 'h :whatever nil :allow-other-keys nil :blah t)
	    (%f 'd 'e 'f :bar 'i :illegal t :foo 'z
		:allow-other-keys nil :zzz 10) ))
  (10 1 nil 2 nil nil nil nil nil)
  (20 40 t 2 nil nil nil nil nil)
  (a b t c t nil nil nil nil)
  (d e t f t nil (:foo h :whatever nil :allow-other-keys nil :blah t) h nil)
  (d e t f t nil (:bar i :illegal t :foo z :allow-other-keys nil :zzz 10) z i))

;;; Tests of non-keyword keywords (see section 3.4.1.4, paragrph 2).
(deftest flet.30
  (flet ((%f (&key ((foo bar) nil)) bar))
    (values (%f) (%f 'foo 10)))
  nil 10)

(deftest flet.31
  (flet ((%f (&key ((:foo bar) nil)) bar))
    (values (%f) (%f :foo 10)))
  nil 10)

;;; Multiple keyword actual parameters
(deftest flet.32
  (flet ((%f (&key a b c) (list a b c)))
    (%f :a 10 :b 20 :c 30 :a 40 :b 50 :c 60))
  (10 20 30))
    
;;; More aux parameters
(deftest flet.33
  (flet ((%f (x y &aux (a (1+ x)) (b (+ x y a)) (c (list x y a b)))
	     c))
    (%f 5 9))
  (5 9 6 20))

(deftest flet.34
  (flet ((%f (x y &rest r &key foo bar &aux (c (list x y r foo bar)))
	     c))
    (values
     (%f 1 2)
     (%f 1 2 :foo 'a)
     (%f 1 2 :bar 'b)
     (%f 1 2 :foo 'a :bar 'b)
     (%f 1 2 :bar 'b :foo 'a)))
  (1 2 nil nil nil)
  (1 2 (:foo a) a nil)
  (1 2 (:bar b) nil b)
  (1 2 (:foo a :bar b) a b)
  (1 2 (:bar b :foo a) a b))

;;; Binding of formal parameters that are also special variables
(deftest flet.35
  (let ((x 'bad))
    (declare (special x))
    (flet ((%f () x))
      (flet ((%g (x)
		 (declare (special x))
		 (%f)))
	(%g 'good))))
  good)

(deftest flet.36
  (let ((x 'bad))
    (declare (special x))
    (flet ((%f () x))
      (flet ((%g (&aux (x 'good))
		 (declare (special x))
		 (%f)))
	 (%g))))
  good)

(deftest flet.37
  (let ((x 'bad))
    (declare (special x))
    (flet ((%f () x))
      (flet ((%g (&rest x)
		 (declare (special x))
		 (%f)))
	 (%g 'good))))
  (good))

(deftest flet.38
  (let ((x 'bad))
    (declare (special x))
    (flet ((%f () x))
      (flet ((%g (&key (x 'good))
		 (declare (special x))
		 (%f)))
	 (%g))))
  good)

(deftest flet.39
  (let ((x 'bad))
    (declare (special x))
    (flet ((%f () x))
      (flet ((%g (&key (x 'bad))
		 (declare (special x))
		 (%f)))
	 (%g :x 'good))))
  good)

(deftest flet.40
  (let ((x 'good))
    (declare (special x))
    (flet ((%f () x))
      (flet ((%g (&key (x 'bad))
		 (%f)))
	 (%g :x 'worse))))
  good)

;;; Test that [:&]allow-other-keys suppress errors for illegal keywords
;;; or odd numbers of keyword arguments

;;; Note -- These are apparently bad tests! -- PFD
;;;(deftest flet.41
;;;  (classify-error
;;;   (flet ((%f (&key (a :good)) a))
;;;     (%f :allow-other-keys t :b)))
;;;  :good)
;;;
;;;(deftest flet.42
;;;  (classify-error
;;;   (flet ((%f (&key (a :good)) a))
;;;     (%f :allow-other-keys t 10 20)))
;;;  :good)
;;;
;;;(deftest flet.43
;;;  (classify-error
;;;   (flet ((%f (&key (a :good) &allow-other-keys) a))
;;;     (%f :b)))
;;;  :good)
;;;
;;;(deftest flet.44
;;;  (classify-error
;;;   (flet ((%f (&key (a :good) &allow-other-keys) a))
;;;     (%f 10 20)))
;;;  :good)


(deftest flet.45
  (flet ((nil () 'a)) (nil))
  a)

(deftest flet.46
  (flet ((t () 'b)) (t))
  b)

;;; Keywords can be function names
(deftest flet.47
  (flet ((:foo () 'bar)) (:foo))
  bar)

(deftest flet.48
  (flet ((:foo () 'bar)) (funcall #':foo))
  bar)

(deftest flet.49
  (loop for s in *cl-non-function-macro-special-operator-symbols*
	for form = `(classify-error (flet ((,s () 'a)) (,s)))
	unless (eq (eval form) 'a)
	collect s)
  nil)

(deftest flet.50
  (loop for s in *cl-non-function-macro-special-operator-symbols*
	for form = `(classify-error (flet ((,s () 'a))
				      (declare (ftype (function () symbol)
						      ,s))
				      (,s)))
	unless (eq (eval form) 'a)
	collect s)
  nil)

;;; Binding SETF functions of certain COMMON-LISP symbols
(deftest flet.51
  (loop for s in *cl-non-function-macro-special-operator-symbols*
	for form = `(classify-error
		     (flet (((setf ,s) (&rest args)
			     (declare (ignore args))
			     'a))
		       (setf (,s) 10)))
	unless (eq (eval form) 'a)
	collect s)
  nil)


