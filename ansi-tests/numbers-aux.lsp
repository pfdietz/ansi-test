;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Apr  7 07:24:43 2003
;;;; Contains: Auxiliary functions for number tests

(in-package :cl-test)

(defun =.4-fn ()
  (loop for x in *numbers*
	append
	(loop for y in *numbers*
	      unless (if (= x y) (= y x) (not (= y x)))
	      collect (list x y))))

(defun /=.4-fn ()
  (loop for x in *numbers*
	append
	(loop for y in *numbers*
	      unless (if (/= x y) (/= y x) (not (/= y x)))
	      collect (list x y))))

(defun /=.4a-fn ()
  (loop for x in *numbers*
	append
	(loop for y in *numbers*
	      when (if (= x y)
		       (/= x y)
		     (not (/= x y)))
	      collect (list x y))))

(defun <.8-fn ()
  (loop for x in *reals*
	nconc
	(loop for y in *reals*
	      when (and (< x y) (> x y))
	      collect (list x y))))

(defun <.9-fn ()
  (loop for x in *reals*
	nconc
	(loop for y in *reals*
	      when (if (< x y) (not (> y x))
		     (> y x))
	      collect (list x y))))

(defun <.10-fn ()
  (loop for x in *reals*
	nconc
	(loop for y in *reals*
	      when (if (< x y) (>= x y)
		     (not (>= x y)))
	      collect (list x y))))

(defun <=.8-fn ()
  (loop for x in *reals*
	nconc
	(loop for y in *reals*
	      when (if (<= x y) (not (>= y x))
		     (>= y x))
	      collect (list x y))))
 
(defun <=.9-fn ()
  (loop for x in *reals*
	nconc
	(loop for y in *reals*
	      when (if (<= x y) (not (or (= x y) (< x y)))
		     (or (= x y) (< x y)))
	      collect (list x y))))

(defun >.8-fn ()
  (loop for x in *reals*
	nconc
	(loop for y in *reals*
	      when (if (> x y) (<= x y)
		     (not (<= x y)))
	      collect (list x y))))

(defun >=.8-fn ()
  (loop for x in *reals*
	nconc
	(loop for y in *reals*
	      when (if (>= x y) (not (or (= x y) (> x y)))
		     (or (= x y) (> x y)))
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
	      for m = (max x y)
	      unless (and (>= m x) (>= m y)
			  (or (= m x) (= m y)))
	      collect (list x y m))))
