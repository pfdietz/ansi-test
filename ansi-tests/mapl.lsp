;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 07:23:23 2003
;;;; Contains: Tests of MAPL

(in-package :cl-test)

(deftest mapl.1
  (mapl #'list nil)
  nil)

(deftest mapl.2
  (let* ((a nil)
	 (x (copy-list '(a b c)))
	 (xcopy (make-scaffold-copy x))
	 (result
	  (mapl #'(lambda (y) (push y a))
		x)))
    (and
     (check-scaffold-copy x xcopy)
     (eqt result x)
     a))
  ((c) (b c) (a b c)))

(deftest mapl.3
  (let* ((a nil)
	 (x (copy-list '(a b c d)))
	 (y (copy-list '(1 2 3 4)))
	 (xcopy (make-scaffold-copy x))
	 (ycopy (make-scaffold-copy y))
	 (result
	  (mapl #'(lambda (xtail ytail)
		    (setf a
			  (append (mapcar #'list xtail ytail)
				  a)))
		x y)))
    (and
     (eqt result x)
     (check-scaffold-copy x xcopy)
     (check-scaffold-copy y ycopy)
     a))
  ((d 4) (c 3) (d 4) (b 2) (c 3) (d 4)
   (a 1) (b 2) (c 3) (d 4)))

(deftest mapl.4
  (let* ((a nil)
	 (x (copy-list '(a b c d)))
	 (y (copy-list '(1 2 3 4 5 6 7 8)))
	 (xcopy (make-scaffold-copy x))
	 (ycopy (make-scaffold-copy y))
	 (result
	  (mapl #'(lambda (xtail ytail)
		    (setf a
			  (append (mapcar #'list xtail ytail)
				  a)))
		x y)))
    (and
     (eqt result x)
     (check-scaffold-copy x xcopy)
     (check-scaffold-copy y ycopy)
     a))
  ((d 4) (c 3) (d 4) (b 2) (c 3) (d 4)
   (a 1) (b 2) (c 3) (d 4)))

(deftest mapl.5
  (let* ((a nil)
	 (x (copy-list '(a b c d e f g)))
	 (y (copy-list '(1 2 3 4)))
	 (xcopy (make-scaffold-copy x))
	 (ycopy (make-scaffold-copy y))
	 (result
	  (mapl #'(lambda (xtail ytail)
		    (setf a
			  (append (mapcar #'list xtail ytail)
				  a)))
		x y)))
    (and
     (eqt result x)
     (check-scaffold-copy x xcopy)
     (check-scaffold-copy y ycopy)
     a))
  ((d 4) (c 3) (d 4) (b 2) (c 3) (d 4)
   (a 1) (b 2) (c 3) (d 4)))

(deftest mapl.order.1
  (let ((i 0) x y z)
    (values
     (mapl (progn
	     (setf x (incf i))
	     (constantly nil))
	   (progn
	     (setf y (incf i))
	     '(a b c))
	   (progn
	     (setf z (incf i))
	     '(1 2 3)))
     i x y z))
  (a b c) 3 1 2 3)

(deftest mapl.error.1
  (classify-error (mapl #'identity 1))
  type-error)

(deftest mapl.error.2
  (classify-error (mapl))
  program-error)

(deftest mapl.error.3
  (classify-error (mapl #'append))
  program-error)

(deftest mapl.error.4
  (classify-error (locally (mapl #'identity 1) t))
  type-error)

(deftest mapl.error.5
  (classify-error (mapl #'cons '(a b c)))
  program-error)

(deftest mapl.error.6
  (classify-error (mapl #'cons '(a b c) '(1 2 3) '(4 5 6)))
  program-error)

(deftest mapl.error.7
  (classify-error (mapl #'caar '(a b c)))
  type-error)

(deftest mapl.error.8
  (classify-error (mapl #'identity (list* (list 1) (list 2) 3)))
  type-error)
