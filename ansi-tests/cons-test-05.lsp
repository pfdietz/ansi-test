;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 28 07:34:08 1998
;;;; Contains:  Testing of CL Features related to "CONS", part 5

(in-package :cl-test)
(use-package :rt)

(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; first, ..., tenth

(deftest first-etc-1
    (let ((x (loop for i from 1 to 20 collect i)))
      (list (first x)
	    (second x)
	    (third x)
	    (fourth x)
	    (fifth x)
	    (sixth x)
	    (seventh x)
	    (eighth x)
	    (ninth x)
	    (tenth x)))
  (1 2 3 4 5 6 7 8 9 10))

(deftest first-etc-2
    (let ((x (make-list 15 :initial-element 'a)))
      (and
       (eql (setf (first x) 1) 1)
       (eql (setf (second x) 2) 2)
       (eql (setf (third x) 3) 3)
       (eql (setf (fourth x) 4) 4)
       (eql (setf (fifth x) 5) 5)
       (eql (setf (sixth x) 6) 6)
       (eql (setf (seventh x) 7) 7)
       (eql (setf (eighth x) 8) 8)
       (eql (setf (ninth x) 9) 9)
       (eql (setf (tenth x) 10) 10)
       x))
  (1 2 3 4 5 6 7 8 9 10 a a a a a))

(deftest rest-set-1
    (let ((x (list 'a 'b 'c)))
      (and
       (eq (setf (rest x) 'd) 'd)
       x))
  (a . d))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setting of C*R accessors

(defun create-c*r-test (n)
  (cond
   ((<= n 0) 'none)
   (t
    (cons (create-c*r-test (1- n))
	  (create-c*r-test (1- n))))))

(loop
    for fn in '(car cdr caar cadr cdar cddr
		caaar caadr cadar caddr cdaar cdadr cddar cdddr
		caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
		cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr)
    do
      (let ((level (- (length (symbol-name fn)) 2)))
	(eval `(deftest ,(intern
			  (concatenate 'string
			    (symbol-name fn)
			    "-SET")
			  :cl-test)
		   (let ((x (create-c*r-test ,level)))
		     (and
		      (setf (,fn x) 'a)
		      (eql (,fn x) 'a)
		      (setf (,fn x) 'none)
		      (equal x (create-c*r-test ,level))
		      ))
		 t))))

(loop
    for (fn len) in '((first 1) (second 2) (third 3) (fourth 4)
		      (fifth 5) (sixth 6) (seventh 7) (eighth 8)
		      (ninth 9) (tenth 10))
    do
      (eval
       `(deftest ,(intern
		   (concatenate 'string
		     (symbol-name fn)
		     "-SET")
		   :cl-test)
	    (let ((x (make-list 20 :initial-element nil)))
	      (and
	       (setf (,fn x) 'a)
	       (loop
		   for i from 1 to 20
		   do (when (and (not (eql i ,len))
				 (nth (1- i) x))
			(return nil))
		   finally (return t))
	       (eql (,fn x) 'a)
	       (nth ,(1- len) x)))
	  a)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nth

(defun nth-1-body (x)
  (loop
      for e in x
       and i from 0
       count (not (eq e (nth i x)))))

(deftest nth-1
    (nth-1-body (loop for i from 1 to 2000 collect (* 4 i)))
  0)

(deftest nth-2
  (handler-case
    (let ((x (loop for i from 1 to 2000 collect i)))
      (loop
	  for i from 0 to 1999 do
	    (setf (nth i x) (- 1999 i)))
      (equal x (loop for i from 1999 downto 0 collect i)))
    (error (c) c))
  t)

#|
(deftest nth-3
  (catch-type-error (nth -1 (copy-tree '(a b c))))
  type-error)
	  
(deftest nth-4
  (catch-type-error (nth 'a (copy-tree '(a b c))))
  type-error)
|#
