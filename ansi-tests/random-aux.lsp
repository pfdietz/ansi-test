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

(defun random-thing (n)
  (if (<= n 1)
      (random-leaf)
    (rcase
     (1 (apply #'cons (mapcar #'random-thing (random-partition (1- n) 2))))
     (1 (apply #'vector (mapcar #'random-thing
				(random-partition (1- n) (max 10 (1- n))))))
     )))

(declaim (special +standard-chars+ *cl-symbols-vector*))

(defparameter *use-random-byte* t)
(defparameter *random-readable* nil)

(defun make-random-string (size-spec &key simple)
  (let*
      ((size (if (eql size-spec '*) (random 30) size-spec))
       (use-random-byte nil)
       (etype 'character)
       (s (random-case
	   (progn
	     (setf use-random-byte *use-random-byte*)
	     (make-string size :element-type 'character))
	   (progn
	     (setf use-random-byte *use-random-byte*)
	     (make-array size :element-type 'character
			 :initial-element #\a))
	   (make-array size :element-type (setf etype (if *random-readable* 'character 'standard-char))
		       :adjustable (and (not simple) (not *random-readable*) (rcase (3 nil) (1 t)))
		       :fill-pointer (and (not simple) (not *random-readable*) (rcase (3 nil) (1 (random (1+ size)))))
		       :initial-element #\a)
	   (make-array size :element-type (setf etype (if *random-readable* 'character 'base-char))
		       :adjustable (and (not simple) (not *random-readable*) (rcase (3 nil) (1 t)))
		       :fill-pointer (and (not simple) (not *random-readable*) (rcase (3 nil) (1 (random (1+ size)))))
		       :initial-element #\a))))
    (if (coin)
	(dotimes (i size)
	  (setf (char s i) (elt #(#\a #\b #\A #\B) (random 4))))
      (dotimes (i size)
	(setf (char s i)
	      (or (and use-random-byte (or (code-char (random (min char-code-limit (ash 1 16))))
					   (code-char (random 256))))
		  (elt "abcdefghijklmnopqrstuvwyxzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
		       (random 62))))))
    (when (and (not simple) (not *random-readable*) (coin 5))
      (let* ((len (length s))
	     (len2 (random (1+ len))))
	(setf s (make-array len2
			    :element-type etype
			    :displaced-to s
			    :displaced-index-offset (random (1+ (- len len2)))))))
      
    s))

(defun random-leaf ()
  (rcase
   (1 (let ((k (ash 1 (1+ (random 40)))))
	(random-from-interval k (- k))))
   (1 (random-from-seq +standard-chars+))
   (1 (random-real))
   (1 (make-random-string (random 20)))
   (1 (gensym))
   (1 (make-symbol (make-random-string (random 20))))
   (1 (random-from-seq *cl-symbols-vector*))))

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



