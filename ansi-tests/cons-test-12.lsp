;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 28 07:38:26 1998
;;;; Contains: Testing of CL Features related to "CONS", part 12

(in-package :cl-test)
(use-package :rt)
(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nthcdr

(deftest nthcdr-1
    (catch-type-error (nthcdr nil (copy-tree '(a b c d))))
  type-error)

(deftest nthcdr-2
    (catch-type-error (nthcdr 'a (copy-tree '(a b c d))))
  type-error)

(deftest nthcdr-3
    (catch-type-error (nthcdr 0.1 (copy-tree '(a b c d))))
  type-error)

(deftest nthcdr-4
    (catch-type-error (nthcdr #\A (copy-tree '(a b c d))))
  type-error)

(deftest nthcdr-5
    (catch-type-error (nthcdr '(a) (copy-tree '(a b c d))))
  type-error)

(deftest nthcdr-6
    (catch-type-error (nthcdr -10 (copy-tree '(a b c d))))
  type-error)

(deftest nthcdr-7
    (nthcdr 0 (copy-tree '(a b c d . e)))
  (a b c d . e))

(deftest nthcdr-8
    (nthcdr 1 (copy-tree '(a b c d)))
  (b c d))

(deftest nthcdr-9
    (nthcdr 10 nil)
  nil)

(deftest nthcdr-10
    (nthcdr 4 (list 'a 'b 'c))
  nil)

(deftest nthcdr-11
    (nthcdr 1 (cons 'a 'b))
  b)

(deftest nthcdr-12
    (catch-type-error (nthcdr 3 (cons 'a 'b)))
  type-error)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rest

(deftest rest-1
    (rest (list 'a 'b 'c))
  (b c))
