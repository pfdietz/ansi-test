;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 28 07:36:01 1998
;;;; Contains: Testing of CL Features related to "CONS", part 8

(in-package :cl-test)
(use-package :rt)

(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Error checking car, cdr, list-length

(deftest car-1
    (catch-type-error (car '(a)))
  a)

(deftest car-nil
    (catch-type-error (car nil))
  nil)

(deftest car-symbol-error
    (catch-type-error (car 'a))
  type-error)

(deftest cdr-1
    (catch-type-error (cdr '(a b)))
  (b))

(deftest cdr-nil
    (catch-type-error (cdr ()))
  nil)

(deftest cdr-symbol-error
    (catch-type-error (cdr 'a))
  type-error)

(deftest list-length-4
    (catch-type-error (list-length (copy-tree '(a b c))))
  3)

(deftest list-length-symbol
    (catch-type-error (list-length 'a))
  type-error)

(deftest list-length-dotted-list
    (catch-type-error (list-length (copy-tree '(a b c d . e))))
  type-error)
