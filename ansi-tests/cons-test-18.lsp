;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 28 10:23:31 1998
;;;; Contains: Testing of CL Features related to "CONS", part 18

(in-package :cl-test)
(use-package :rt)
(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; get-properties

(deftest get-properties-1
    (get-properties nil nil)
  nil nil nil)

(deftest get-properties-2
    (get-properties '(a b) nil)
  nil nil nil)

(deftest get-properties-3
    (get-properties '(a b c d) '(a))
  a b (a b c d))

(deftest get-properties-4
    (get-properties '(a b c d) '(c))
  c d (c d))

(deftest get-properties-5
    (get-properties '(a b c d) '(c a))
  a b (a b c d))

(deftest get-properties-6
    (get-properties '(a b c d) '(b))
  nil nil nil)

(deftest get-properties-7
    (get-properties '("aa" b c d) (list (copy-seq "aa")))
  nil nil nil)

(deftest get-properties-8
    (get-properties '(1000000000000 b c d) (list (1+ 999999999999)))
  nil nil nil)

(deftest get-properties-9
    (let* ((x (copy-list '(a b c d e f g h a c)))
	   (xcopy (make-scaffold-copy x))
	   (y (copy-list '(x y f g)))
	   (ycopy (make-scaffold-copy y)))
      (multiple-value-bind
	  (indicator value tail)
	  (get-properties x y)
	(and
	 (check-scaffold-copy x xcopy)
	 (check-scaffold-copy y ycopy)
	 (eq tail (nthcdr 6 x))
	 (values indicator value tail))))
  g h (g h a c))

	   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; getf

(deftest getf-1
    (getf nil 'a)
  nil)

(deftest getf-2
    (getf nil 'a 'b)
  b)

(deftest getf-3
    (getf '(a b) 'a)
  b)

(deftest getf-4
    (getf '(a b) 'a 'c)
  b)

(deftest getf-5
    (let ((x 0))
      (values
       (getf '(a b) 'a (incf x))
       x))
  b 1)

(deftest setf-getf-1
    (let ((p (copy-list '(a 1 b 2))))
      (setf (getf p 'c) 3)
      ;; Must check that only a, b, c have properties
      (and
       (eql (getf p 'a) 1)
       (eql (getf p 'b) 2)
       (eql (getf p 'c) 3)
       (eql
	(loop
	    for ptr on p by #'cddr count
	      (not (member (car ptr) '(a b c))))
	0)
       t))
  t)

(deftest setf-getf-2
    (let ((p (copy-list '(a 1 b 2))))
      (setf (getf p 'a) 3)
      ;; Must check that only a, b have properties
      (and
       (eql (getf p 'a) 3)
       (eql (getf p 'b) 2)
       (eql
	(loop
	    for ptr on p by #'cddr count
	      (not (member (car ptr) '(a b))))
	0)
       t))
  t)    
       
(deftest setf-getf-3
    (let ((p (copy-list '(a 1 b 2))))
      (setf (getf p 'c 17) 3)
      ;; Must check that only a, b, c have properties
      (and
       (eql (getf p 'a) 1)
       (eql (getf p 'b) 2)
       (eql (getf p 'c) 3)
       (eql
	(loop
	    for ptr on p by #'cddr count
	      (not (member (car ptr) '(a b c))))
	0)
       t))
  t)

(deftest setf-getf-4
    (let ((p (copy-list '(a 1 b 2))))
      (setf (getf p 'a 17) 3)
      ;; Must check that only a, b have properties
      (and
       (eql (getf p 'a) 3)
       (eql (getf p 'b) 2)
       (eql
	(loop
	    for ptr on p by #'cddr count
	      (not (member (car ptr) '(a b))))
	0)
       t))
  t)

(deftest setf-getf-5
    (let ((p (copy-list '(a 1 b 2)))
	  (foo nil))
      (setf (getf p 'a (progn (setf foo t) 0)) 3)
      ;; Must check that only a, b have properties
      (and
       (eql (getf p 'a) 3)
       (eql (getf p 'b) 2)
       (eql
	(loop
	    for ptr on p by #'cddr count
	      (not (member (car ptr) '(a b))))
	0)
       foo))
  t)

(deftest incf-getf-1
    (let ((p (copy-list '(a 1 b 2))))
      (incf (getf p 'b))
      ;; Must check that only a, b have properties
      (and
       (eql (getf p 'a) 1)
       (eql (getf p 'b) 3)
       (eql
	(loop
	    for ptr on p by #'cddr count
	      (not (member (car ptr) '(a b))))
	0)
       t))
  t)

(deftest incf-getf-2
    (let ((p (copy-list '(a 1 b 2))))
      (incf (getf p 'c 19))
      ;; Must check that only a, b have properties
      (and
       (eql (getf p 'a) 1)
       (eql (getf p 'b) 2)
       (eql (getf p 'c) 20)
       (eql
	(loop
	    for ptr on p by #'cddr count
	      (not (member (car ptr) '(a b c))))
	0)
       t))
  t)

(deftest push-getf-1
    (let ((p nil))
      (values
       (push 'x (getf p 'a))
       p))
  (x) (a (x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; remf

(deftest remf-1
    (let ((x nil))
      (values (remf x 'a) x))
  nil ())

(deftest remf-2
    (let ((x (list 'a 'b)))
      (values (not (null (remf x 'a))) x))
  t ())

(deftest remf-3
    (let ((x (list 'a 'b 'a 'c)))
      (values (not (null (remf x 'a))) x))
  t (a c))

(deftest remf-4
    (let ((x (list 'a 'b 'c 'd)))
      (values
       (and (remf x 'c) t)
       (loop
	   for ptr on x by #'cddr count
	     (not (eq (car ptr) 'a)))))
  t 0)





      