;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 22:41:14 2003
;;;; Contains: Tests of BUTLAST

(in-package :cl-test)

(deftest butlast.1
  (let ((x (list 'a 'b 'c 'd 'e)))
    (let ((xcopy (make-scaffold-copy x)))
      (let ((result (butlast x 2)))
	(and
	 (check-scaffold-copy x xcopy)
	 result))))
  (a b c))

(deftest butlast.2
  (let ((x (list 'a 'b 'c 'd 'e)))
    (let ((xcopy (make-scaffold-copy x)))
      (let ((result (butlast x 0)))
	(and
	 (check-scaffold-copy x xcopy)
	 result))))
  (a b c d e))

(deftest butlast.3
  (let ((x (list 'a 'b 'c 'd 'e)))
    (let ((xcopy (make-scaffold-copy x)))
      (let ((result (butlast x 5)))
	(and
	 (check-scaffold-copy x xcopy)
	 result))))
  nil)

(deftest butlast.4
  (let ((x (list 'a 'b 'c 'd 'e)))
    (let ((xcopy (make-scaffold-copy x)))
      (let ((result (butlast x 6)))
	(and
	 (check-scaffold-copy x xcopy)
	 result))))
  nil)

(deftest butlast.5
  (butlast (copy-tree '(a b c . d)) 1)
  (a b))

(deftest butlast.order.1
  (let ((i 0) x y)
    (values
     (butlast (progn (setf x (incf i))
		     (list 'a 'b 'c 'd 'e))
	      (progn (setf y (incf i))
		     2))
     i x y))
  (a b c) 2 1 2)

(deftest butlast.order.2
  (let ((i 0))
    (values
     (butlast (progn (incf i) '(a b c d)))
     i))
  (a b c) 1)

(deftest butlast.error.1
  (classify-error (butlast (copy-tree '(a b c d)) 'a))
  type-error)

(deftest butlast.error.2
  (classify-error (butlast 'a 0))
  type-error)

(deftest butlast.error.3
  (classify-error (butlast))
  program-error)

(deftest butlast.error.4
  (classify-error (butlast '(a b c) 3 3))
  program-error)

(deftest butlast.error.5
  (classify-error (locally (butlast 'a 0) t))
  type-error)

