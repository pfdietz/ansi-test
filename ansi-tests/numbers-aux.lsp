;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Apr  7 07:24:43 2003
;;;; Contains: Auxiliary functions for number tests

(in-package :cl-test)

(eval-when (load eval)
  (compile-and-load "random-aux.lsp"))

(defun eqlzt (x y)
  "Return T if (eql x y) or if both are zero of the same type."
  (cond
   ((complexp x)
    (and (complexp y)
	 (eqlzt (realpart x) (realpart y))
	 (eqlzt (imagpart x) (imagpart y))))
   ((zerop x)
    (eqlt (abs x) (abs y)))
   (t (eqlt x y))))

(defun numbers-are-compatible (x y)
  (cond
   ((complexp x)
    (and (numbers-are-compatible (realpart x) y)
	 (numbers-are-compatible (imagpart x) y)))
   ((complexp y)
    (and (numbers-are-compatible x (realpart y))
	 (numbers-are-compatible x (imagpart y))))
   (t
    (when (floatp x) (rotatef x y))
    (or (floatp x)
	(not (floatp y))
	(etypecase y
	  (short-float
	   (<= #.(rational most-negative-short-float)
	       x
	       #.(rational most-positive-short-float)))
	  (single-float
	   (<= #.(rational most-negative-single-float)
	       x
	       #.(rational most-positive-single-float)))
	  (double-float
	   (<= #.(rational most-negative-double-float)
	       x
	       #.(rational most-positive-double-float)))
	  (long-float
	   (<= #.(rational most-negative-long-float)
	       x
	       #.(rational most-positive-long-float))))))))

;;; NOTE!  According to section 12.1.4.1, when a rational is compared
;;; to a float, the effect is as if the float is convert to a rational
;;; (by RATIONAL), not as if the rational is converted to a float.
;;; This means the calls to numbers-are-compatible are not necessary.

(defun =.4-fn ()
  (loop for x in *numbers*
	append
	(loop for y in *numbers*
	      unless (or ;; (not (numbers-are-compatible x y))
			 (if (= x y) (= y x) (not (= y x))))
	      collect (list x y))))

(defun /=.4-fn ()
  (loop for x in *numbers*
	append
	(loop for y in *numbers*
	      unless (or ;; (not (numbers-are-compatible x y))
			 (if (/= x y) (/= y x) (not (/= y x))))
	      collect (list x y))))

(defun /=.4a-fn ()
  (loop for x in *numbers*
	append
	(loop for y in *numbers*
	      when (and ;; (numbers-are-compatible x y)
			(if (= x y)
			    (/= x y)
			  (not (/= x y))))
	      collect (list x y))))

(defun <.8-fn ()
  (loop for x in *reals*
	nconc
	(loop for y in *reals*
	      when (and ;; (numbers-are-compatible x y)
			(and (< x y) (> x y)))
	      collect (list x y))))

(defun <.9-fn ()
  (loop for x in *reals*
	nconc
	(loop for y in *reals*
	      when (and ;; (numbers-are-compatible x y)
			(if (< x y) (not (> y x))
			  (> y x)))
	      collect (list x y))))

(defun <.10-fn ()
  (loop for x in *reals*
	nconc
	(loop for y in *reals*
	      when (and ;; (numbers-are-compatible x y)
			(if (< x y) (>= x y)
			  (not (>= x y))))
	      collect (list x y))))

(defun <=.8-fn ()
  (loop for x in *reals*
	nconc
	(loop for y in *reals*
	      when (and ;; (numbers-are-compatible x y)
			(if (<= x y) (not (>= y x))
			  (>= y x)))
	      collect (list x y))))
 
(defun <=.9-fn ()
  (loop for x in *reals*
	nconc
	(loop for y in *reals*
	      when (and ;; (numbers-are-compatible x y)
			(if (<= x y) (not (or (= x y) (< x y)))
			  (or (= x y) (< x y))))
	      collect (list x y))))

(defun >.8-fn ()
  (loop for x in *reals*
	nconc
	(loop for y in *reals*
	      when (and ;; (numbers-are-compatible x y)
			(if (> x y) (<= x y)
			  (not (<= x y))))
	      collect (list x y))))

(defun >=.8-fn ()
  (loop for x in *reals*
	nconc
	(loop for y in *reals*
	      when (and ;; (numbers-are-compatible x y)
			(if (>= x y) (not (or (= x y) (> x y)))
			  (or (= x y) (> x y))))
	      collect (list x y))))

;;; Comparison of rationsls

(defun compare-random-rationals (n m rep)
  (loop for a = (- (random n) m)
	for b = (- (random n) m)
	for c = (- (random n) m)
	for d = (- (random n) m)
	repeat rep
	when
	(and (/= b 0)
	     (/= d 0)
	     (let ((q1 (/ a b))
		   (q2 (/ c d))
		   (ad (* a d))
		   (bc (* b c)))
	       (when (< (* b d) 0)
		 (setq ad (- ad))
		 (setq bc (- bc)))
	       (or (if (< q1 q2) (not (< ad bc)) (< ad bc))
		   (if (<= q1 q2) (not (<= ad bc)) (<= ad bc))
		   (if (> q1 q2) (not (> ad bc)) (> ad bc))
		   (if (>= q1 q2) (not (>= ad bc)) (>= ad bc))
		   (if (= q1 q2) (not (= ad bc)) (= ad bc))
		   (if (/= q1 q2) (not (/= ad bc)) (/= ad bc)))))
	collect (list a b c d)))

(defun max.2-fn ()
  (loop for x in *reals*
	nconc
	(loop for y in *reals*
	      when (numbers-are-compatible x y)
	      unless (let ((m (max x y)))
		       (and (>= m x) (>= m y)
			    (or (= m x) (= m y))))
	      collect (list x y (max x y)))))

(defun min.2-fn ()
  (loop for x in *reals*
	nconc
	(loop for y in *reals*
	      when (numbers-are-compatible x y)
	      unless (let ((m (min x y)))
		       (and (<= m x) (<= m y)
			    (or (= m x) (= m y))))
	      collect (list x y (min x y)))))

(defun epsilon (number)
  (etypecase number
    (short-float short-float-epsilon)
    (single-float single-float-epsilon)
    (double-float double-float-epsilon)
    (long-float long-float-epsilon)
    (rational 0)))

(defun negative-epsilon (number)
  (etypecase number
    (short-float short-float-negative-epsilon)
    (single-float single-float-negative-epsilon)
    (double-float double-float-negative-epsilon)
    (long-float long-float-negative-epsilon)
    (rational 0)))

;;; Compute the number of digits that can be added to 1.0 in the appropriate
;;; float type, a rational representation of the smallest radix^(-k) s.t.
;;; 1.0 + radix^(-k) /= 1.0, and the float representation of that value.
;;; Note that this will in general be > <float-type>-epsilon.

(defun find-epsilon (x)
  (assert (floatp x))
  (let* ((one (float 1 x))
	 (radix (float-radix one))
	 (eps (/ 1 radix)))
    (loop
     for next-eps = (/ eps radix)
     for i from 1
     until (eql one (+ one next-eps))
     do (setq eps next-eps)
     finally (return (values i eps (float eps one))))))

;;; Approximate equality function
(defun approx= (x y &optional (eps (epsilon x)))
  (<= (abs (/ (- x y) (max (abs x) 1))) eps))

(defun test-log-op-with-decls (op xlo xhi ylo yhi niters
				  &optional
				  (decls '((optimize (speed 3) (safety 1)
						     (debug 1)))))
  "Test that a compiled form of the LOG* function OP computes
   the expected result on two random integers drawn from the
   types `(integer ,xlo ,xhi) and `(integer ,ylo ,yhi).  Try
   niters choices.  Return a list of pairs on which the test fails."

  (assert (symbolp op))
  (assert (integerp xlo))
  (assert (integerp xhi))
  (assert (integerp ylo))
  (assert (integerp yhi))
  (assert (integerp niters))
  (assert (<= xlo xhi))
  (assert (<= ylo yhi))

  (let* ((source
	  `(lambda (x y)
	     (declare (type (integer ,xlo ,xhi) x)
		      (type (integer ,ylo ,yhi) y)
		      ,@ decls)
	     (,op x y)))
	 (fn (compile nil source)))
    (loop for i below niters
	  for x = (random-from-interval (1+ xhi) xlo)
	  for y = (random-from-interval (1+ yhi) ylo)
	  unless (eql (funcall (the symbol op) x y)
		      (funcall fn x y))
	  collect (list x y))))

(defun test-log-op (op n1 n2)
  (flet ((%r () (let ((r (random 33)))
		  (- (random (ash 1 (1+ r))) (ash 1 r)))))
    (loop for x1 = (%r)
	  for x2 = (%r)
	  for y1 = (%r)
	  for y2 = (%r)
	  repeat n1
	  nconc
	  (test-log-op-with-decls op
				  (min x1 x2) (max x1 x2)
				  (min y1 y2) (max y1 y2)
				  n2))))
