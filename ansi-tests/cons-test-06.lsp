;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 28 07:34:40 1998
;;;; Contains: Testing of CL Features related to "CONS", part 6

(in-package :cl-test)
(use-package :rt)

(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; endp

(deftest endp-nil
    (not (not (endp nil)))
  t)

(deftest endp-cons
    (endp (cons 'a 'a))
  nil)

(deftest endp-singleton-list
    (endp '(a))
  nil)

(deftest endp-symbol-error
    (catch-type-error (endp 'a))
  type-error)

(deftest endp-fixnum-error
    (catch-type-error (endp 1))
  type-error)

(deftest endp-float-error
    (catch-type-error (endp 0.9212d4))
  type-error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; null

(deftest null-1
    (null nil)
  t)

(deftest null-2
    (not (some #'null
	       `(1 a 1.2 "a" #\w (a) ,*terminal-io*
		   #'car (make-array '(10)))))
  t)

(deftest not-1
    (not nil)
  t)

(deftest not-2
    (not (some #'not
	       `(1 a 1.2 "a" #\w (a) ,*terminal-io*
		   #'car (make-array '(10)))))
  t)

(deftest typep-nil-null
    (not (not (typep nil 'null)))
  t)

(deftest typep-t-null
    (typep t 'null)
  nil)
