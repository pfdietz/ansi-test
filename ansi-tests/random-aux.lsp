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

(defun random-from-interval (lo hi)
  "Generate random value from interval [lo,hi)"
  (assert (> hi lo))
  (+ (random (- hi lo)) lo))

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



