;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 07:24:00 2003
;;;; Contains: Tests of MAPLIST

(in-package :cl-test)

(deftest maplist.1
  (maplist #'list nil)
  nil)

(deftest maplist.2
  (let* ((x (copy-list '(a b c)))
	 (xcopy (make-scaffold-copy x))
	 (result (maplist #'identity x)))
    (and (check-scaffold-copy x xcopy)
	 result))
  ((a b c) (b c) (c)))

(deftest maplist.3
  (let* ((x (copy-list '(a b c d)))
	 (y (copy-list '(1 2 3 4)))
	 (xcopy (make-scaffold-copy x))
	 (ycopy (make-scaffold-copy y))
	 (result
	  (maplist #'append x y)))
    (and
     (check-scaffold-copy x xcopy)
     (check-scaffold-copy y ycopy)
     result))
  ((a b c d 1 2 3 4)
   (b c d 2 3 4)
   (c d 3 4)
   (d 4)))

(deftest maplist.4
  (let* ((x (copy-list '(a b c d)))
	 (y (copy-list '(1 2 3 4 5)))
	 (xcopy (make-scaffold-copy x))
	 (ycopy (make-scaffold-copy y))
	 (result
	  (maplist #'append x y)))
    (and
     (check-scaffold-copy x xcopy)
     (check-scaffold-copy y ycopy)
     result))
  ((a b c d 1 2 3 4 5)
   (b c d 2 3 4 5)
   (c d 3 4 5)
   (d 4 5)))

(deftest maplist.5
  (let* ((x (copy-list '(a b c d e)))
	 (y (copy-list '(1 2 3 4)))
	 (xcopy (make-scaffold-copy x))
	 (ycopy (make-scaffold-copy y))
	 (result
	  (maplist #'append x y)))
    (and
     (check-scaffold-copy x xcopy)
     (check-scaffold-copy y ycopy)
     result))
  ((a b c d e 1 2 3 4)
   (b c d e 2 3 4)
   (c d e 3 4)
   (d e 4)))

(deftest maplist.6
  (maplist 'append '(a b c) '(1 2 3))
  ((a b c 1 2 3) (b c 2 3) (c 3)))

(deftest maplist.7
  (maplist #'(lambda (x y) (nth (car x) y))
	   '(0 1 0 1 0 1 0)
	   '(a b c d e f g)
	   )
  (a c c e e g g))

(deftest maplist.order.1
  (let ((i 0) x y z)
    (values
     (maplist
      (progn
	(setf x (incf i))
	#'(lambda (x y) (declare (ignore x)) (car y)))
      (progn
	(setf y (incf i))
	'(a b c))
      (progn
	(setf z (incf i))
	     '(1 2 3)))
     i x y z))
  (1 2 3) 3 1 2 3)

(deftest maplist.error.1
  (classify-error (maplist #'identity 'a))
  type-error)

(deftest maplist.error.2
  (classify-error (maplist #'identity 1))
  type-error)

(deftest maplist.error.3
  (classify-error (maplist #'identity 1.1323))
  type-error)

(deftest maplist.error.4
  (classify-error (maplist #'identity "abcde"))
  type-error)

(deftest maplist.error.5
  (classify-error (maplist))
  program-error)

(deftest maplist.error.6
  (classify-error (maplist #'append))
  program-error)

(deftest maplist.error.7
  (classify-error (locally (maplist #'identity 'a) t))
  type-error)

(deftest maplist.error.8
  (classify-error (maplist #'caar '(a b c)))
  type-error)

(deftest maplist.error.9
  (classify-error (maplist #'cons '(a b c)))
  program-error)

(deftest maplist.error.10
  (classify-error (maplist #'cons '(a b c) '(1 2 3) '(4 5 6)))
  program-error)

(deftest maplist.error.11
  (classify-error (maplist #'identity (list* (list 1) (list 2) 3)))
  type-error)

