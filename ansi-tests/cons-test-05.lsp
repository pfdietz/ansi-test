;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 28 07:34:08 1998
;;;; Contains:  Testing of CL Features related to "CONS", part 5

(in-package :cl-test)

(declaim (optimize (safety 3)))

(defparameter *cons-accessors*
  '(first second third fourth fifth sixth seventh eighth ninth tenth
    car cdr caar cadr cdar cddr
    caaar caadr cadar caddr cdaar cdadr cddar cdddr
    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
    cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr))

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
     (eqt (setf (rest x) 'd) 'd)
     x))
  (a . d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setting of C*R accessors

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
	    (let ((x (create-c*r-test ,level))
		  (y (list (create-c*r-test ,level)))
		  (i 0))
	      (and
	       (setf (,fn (progn (incf i) x)) 'a)
	       (eqlt (,fn x) 'a)
	       (eqlt i 1)
	       (setf (,fn x) 'none)
	       (equalt x (create-c*r-test ,level))
	       (setf (,fn (progn (incf i) (car y))) 'a)
	       (eqlt (,fn (car y)) 'a)
	       (eqlt i 2)
	       (setf (,fn (car y)) 'none)
	       (null (cdr y))
	       (equalt (car y) (create-c*r-test ,level))
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
     (let* ((x (make-list 20 :initial-element nil))
	    (y (list (copy-list x)))
	    (cnt 0))
       (and
	(setf (,fn (progn (incf cnt) x)) 'a)
	(eqlt cnt 1)
	(loop
	 for i from 1 to 20
	 do (when (and (not (eql i ,len))
		       (nth (1- i) x))
	      (return nil))
	 finally (return t))
	(setf (,fn (car y)) 'a)
	(loop
	 for i from 1 to 20
	 do (when (and (not (eql i ,len))
		       (nth (1- i) (car y)))
	      (return nil))
	 finally (return t))
	(eqlt (,fn x) 'a)
	(eqlt (nth ,(1- len) x) 'a)
	(eqlt (,fn (car y)) 'a)
	(nth ,(1- len) (car y))))
     a)))

;; set up program error tests

(loop for name in *cons-accessors*
      do (eval
	  `(deftest ,(intern (concatenate 'string (symbol-name name)
					  ".ERROR.NO-ARGS")
			     :cl-test)
	     (classify-error (,name))
	     program-error))
      do (eval
	  `(deftest ,(intern (concatenate 'string (symbol-name name)
					  ".ERROR.EXCESS-ARGS")
			     :cl-test)
	     (classify-error (,name nil nil))
	     program-error)))	  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nth

(deftest nth.1
  (nth-1-body (loop for i from 1 to 2000 collect (* 4 i)))
  0)

(deftest nth.2
  (let ((x (loop for i from 1 to 2000 collect i)))
    (loop
     for i from 0 to 1999 do
     (setf (nth i x) (- 1999 i)))
    (equalt x (loop for i from 1999 downto 0 collect i)))
  t)

;;; Test side effects, evaluation order in assignment to NTH
(deftest nth.order.1
  (let ((i 0)
	(x (list 'a 'b 'c 'd))
	y z)
    (and
     (eqlt (setf (nth (setf y (incf i)) x) (progn (setf z (incf i)) 'z))
	   'z)
     (eqlt y 1)
     (eqlt z 2)
     x))
  (a z c d))

(deftest nth.order.2
  (let ((i 0) x y (z '(a b c d e)))
    (values
     (nth (progn (setf x (incf i)) 1)
	  (progn (setf y (incf i)) z))
     i x y))
  b 2 1 2)

(deftest nth.error.1
  (classify-error (nth))
  program-error)

(deftest nth.error.2
  (classify-error (nth 0))
  program-error)

(deftest nth.error.3
  (classify-error (nth 1 '(a b c) nil))
  program-error)

(deftest nth.error.4
  (classify-error (nth 0 '(a b c) nil))
  program-error)
