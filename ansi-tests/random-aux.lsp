;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jun  8 06:56:15 2003
;;;; Contains: Aux. functions and macros used for randomization

(in-package :cl-test)

(defun random-from-seq (seq)
  "Generate a random member of a sequence."
  (let ((len (length seq)))
    (assert (> len 0))
    (elt seq (random len))))

(defmacro random-case (&body cases)
  (let ((len (length cases)))
    (assert (> len 0))
    `(case (random ,len)
       ,@(loop for i from 0 for e in cases collect `(,i ,e))
       (t (error "Can't happen?! (in random-case)~%")))))

(defmacro rcase (&body cases)
  "Usage: (RCASE (<weight> <form>+)+), where <weight> is a positive real
   indicating the relative probability of executing the associated implicit
   progn."
  (assert cases)
  (let* ((weights (mapcar #'car cases))
	 (cumulative-weights (let ((sum 0))
			       (loop for w in weights collect (incf sum w))))
	 (total (car (last cumulative-weights)))
	 (r (gensym)))
    (assert (every #'plusp weights))
    `(let ((,r (random ,total)))
       (cond
	,@(loop for case in (butlast cases)
		for cw in cumulative-weights
		collect `((< ,r ,cw) ,@(cdr case)))
	(t ,@(cdar (last cases)))))))

(defun random-nonnegative-real ()
  (if (coin 3)
      (random-case
       (/ (random 10000) (1+ (random 1000)))
       (/ (random 1000000) (1+ (random 100000)))
       (/ (random 100000000) (1+ (random 10000000)))
       (/ (random 1000000000000) (1+ (random 10000000))))
    (random (random-case
	     1000
	     100000
	     10000000
	     1000000000
	     (expt 2.0s0 (random 15))
	     (expt 2.0f0 (random 32))
	     (expt 2.0d0 (random 32))
	     (expt 2.0l0 (random 32))))))

(defun random-real ()
  (if (coin) (random-nonnegative-real)
    (- (random-nonnegative-real))))

(defun random-fixnum ()
  (+ (random (1+ (- most-positive-fixnum most-negative-fixnum)))
     most-negative-fixnum))

(defun random-from-interval (upper &optional (lower (- upper)))
  (+ (random (- upper lower)) lower))

(defun coin (&optional (n 2))
  "Flip an n-sided coin."
  (eql (random n) 0))

;;; Randomly permute a sequence
(defun random-permute (seq)
  (setq seq (copy-seq seq))
  (let ((len (length seq)))
    (loop for i from len downto 2
	  do (let ((r (random i)))
	       (rotatef (elt seq r) (elt seq (1- i))))))
  seq)

(defun binomial-distribution-test (n fn)
  (let* ((count (loop repeat n count (funcall fn)))
	 (sigma (/ (sqrt n) 2.0))
	 (bound (* sigma 6))
	 (expected (/ n 2.0)))
    (<= (- expected bound)
	count
	(+ expected bound))))



