;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Oct  6 05:04:45 2003
;;;; Contains: Generating random types and testing relationships on them

(in-package :cl-test)

(compile-and-load "random-aux.lsp")
(compile-and-load "random-int-form.lsp")

(defun make-random-type (size)
  (if (<= size 1)
      (rcase
       (1 nil)
       (1 t)
       (1 `(eql ,(let ((r (ash 1 (random 45))))
		   (random-from-interval r (- r)))))
       (1 (random-from-seq #(integer unsigned-byte ratio rational real float
			     short-float single-float double-float
			     long-float complex symbol cons)))
       (1
	(let* ((len (random *maximum-random-int-bits*))
	       (r1 (ash 1 len))
	       (r2 (+ r1 r1))
	       (x (- (random r2) r1))
	       (y (- (random r2) r1))
	       (lo (min x y))
	       (hi (max x y)))
	  `(integer ,lo ,hi)))
       )
    (rcase
     (2 (destructuring-bind (s1 s2)
	    (random-partition (1- size) 2)
	  (let ((op (random-from-seq #(cons cons and or))))
	    `(,op ,(make-random-type s1)
		  ,(make-random-type s2)))))
     (1 `(not ,(make-random-type (1- size))))
     )))

(defun test-random-types (n size)
  (loop for t1 = (make-random-type size)
	for t2 = (make-random-type size)
	for i from 0 below n
	;; do (print (list t1 t2))
	do (when (and (= (mod i 100) 0) (> i 0))
	     (format t "~A " i) (finish-output *standard-output*))
	when (test-types t1 t2)
	collect (list t1 t2)
	finally (terpri)))
  

(defun test-types (t1 t2)
  (multiple-value-bind (sub success)
      (subtypep t1 t2)
    (when success
      (if sub
	  (check-all-subtypep t1 t2)
	(let ((nt1 `(not ,t1))
	      (nt2 `(not ,t2)))
	  (subtypep nt2 nt1))))))
