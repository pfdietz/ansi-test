;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 28 07:35:15 1998
;;;; Contains: Testing of CL Features related to "CONS", part 7

(in-package :cl-test)
(use-package :rt)

(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nconc

(deftest nconc-1
    (nconc)
  nil)

(deftest nconc-2
    (nconc (copy-tree '(a b c d e f)))
  (a b c d e f))

(deftest nconc-3
    (nconc 1)
  1)

(deftest nconc-4
    (let ((x (list 'a 'b 'c))
	  (y (list 'd 'e 'f)))
      (let ((ycopy (make-scaffold-copy y)))
	(let ((result (nconc x y)))
	  (and
	   (check-scaffold-copy y ycopy)
	   (eq (cdddr x) y)
	   result))))
  (a b c d e f))

(deftest nconc-5
    (let ((x (list 'a 'b 'c)))
      (nconc x x)
      (and
       (eq (cdddr x) x)
       (null (list-length x))))
  t)

(deftest nconc-6
    (let ((x (list 'a 'b 'c))
	  (y (list 'd 'e 'f 'g 'h))
	  (z (list 'i 'j 'k)))
      (let ((result (nconc x y z 'foo)))
	(and
	 (eq (nthcdr 3 x) y)
	 (eq (nthcdr 5 y) z)
	 (eq (nthcdr 3 z) 'foo)
	 result)))
  (a b c d e f g h i j k . foo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; append

(deftest append-1
    (append)
  nil)

(deftest append-2
    (append 'x)
  x)

(deftest append-3
    (let ((x (list 'a 'b 'c 'd))
	  (y (list 'e 'f 'g)))
      (let ((xcopy (make-scaffold-copy x))
	    (ycopy (make-scaffold-copy y)))
	(let ((result (append x y)))
	  (and
	   (check-scaffold-copy x xcopy)
	   (check-scaffold-copy y ycopy)
	   result))))
  (a b c d e f g))

(deftest append-4
    (append (list 'a) (list 'b) (list 'c)
	    (list 'd) (list 'e) (list 'f)
	    (list 'g) 'h)
  (a b c d e f g . h))

(deftest append-5
    (append nil nil nil nil nil nil nil nil 'a)
  a)

#|
(defun append-6-body ()
  (let ((step (max 1 (min 256 (floor (/ call-arguments-limit 64))))))
    (loop
	for n from 0
	to call-arguments-limit
	by step
	count
	  (not
	   (equal
	    (apply #'append (loop for i from 1 to n
				collect '(a)))
	    (make-list n :initial-element 'a))))))
(deftest append-6
    (cons-178-body)
  0)
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; revappend

(deftest revappend-1
    (let* ((x (list 'a 'b 'c))
	   (y (list 'd 'e 'f))
	   (xcopy (make-scaffold-copy x))
	   (ycopy (make-scaffold-copy y))
	   )
      (let ((result (revappend x y)))
	(and
	 (check-scaffold-copy x xcopy)
	 (check-scaffold-copy y ycopy)
	 (eq (cdddr result) y)
	 result)))
  (c b a d e f))

(deftest revappend-2
    (revappend (copy-tree '(a b c d e)) 10)
  (e d c b a . 10))

(deftest revappend-3
    (revappend nil 'a)
  a)

(deftest revappend-4
    (revappend (copy-tree '(a (b c) d)) nil)
  (d (b c) a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nreconc

(deftest nreconc-1
  (let* ((x (list 'a 'b 'c))
	 (y (copy-tree '(d e f)))
	 (result (nreconc x y)))
    (and (equal y '(d e f))
	 result))
  (c b a d e f))

(deftest nreconc-2
  (nreconc nil 'a)
  a)

