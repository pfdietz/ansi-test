;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 28 07:39:29 1998
;;;; Contains: Testing of CL Features related to "CONS", part 14

(in-package :cl-test)
(use-package :rt)
(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; member-if

(deftest member-if-1
  (member-if #'listp nil)
  nil)

(deftest member-if-2
  (member-if #'(lambda (x) (eqt x 'a)) '(1 2 a 3 4))
  (a 3 4))

(deftest member-if-3
  (member-if #'(lambda (x) (eql x 12)) '(4 12 11 73 11) :key #'1+)
  (11 73 11))

(deftest member-if-4
  (let ((test-inputs
	 `(1 a 11.3121 11.31s3 1.123f5 -1 0
	     13.13122d34 581.131e-10
	     (a b c . d)
	     ,(make-array '(10))
	     "ancadas"  #\w)))
    (not (every
	  #'(lambda (x)
	      (let ((result (catch-type-error (member-if #'listp x))))
		(or (eqt result 'type-error)
		    (progn
		      (format t "~%On ~S: returned ~%~S" x result)
		      nil))))
	  test-inputs)))
  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; member-if-not

(deftest member-if-not-1
  (member-if-not #'listp nil)
  nil)

(deftest member-if-not-2
  (member-if-not #'(lambda (x) (eqt x 'a)) '(a 1 2 a 3 4))
  (1 2 a 3 4))

(deftest member-if-not-3
  (member-if-not #'(lambda (x) (not (eql x 12))) '(4 12 11 73 11) :key #'1+)
  (11 73 11))

(deftest member-if-not-4
  (let ((test-inputs
	 `(1 a 11.3121 11.31s3 1.123f5 -1 0
	     13.13122d34 581.131e-10
	     ((a) (b) (c) . d)
	     ,(make-array '(10))
	     "ancadas"  #\w)))
    (not (every
	  #'(lambda (x)
	      (let ((result (catch-type-error (member-if-not #'listp x))))
		(or (eqt result 'type-error)
		    (progn
		      (format t "~%On x = ~S, returns: ~%~S" x result)
		      nil))))
	  test-inputs)))
  nil)
