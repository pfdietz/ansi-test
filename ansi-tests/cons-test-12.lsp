;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 28 07:38:26 1998
;;;; Contains: Testing of CL Features related to "CONS", part 12

(in-package :cl-test)
(use-package :rt)
(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nthcdr

(deftest nthcdr.error.1
  (catch-type-error (nthcdr nil (copy-tree '(a b c d))))
  type-error)

(deftest nthcdr.error.2
  (catch-type-error (nthcdr 'a (copy-tree '(a b c d))))
  type-error)

(deftest nthcdr.error.3
  (catch-type-error (nthcdr 0.1 (copy-tree '(a b c d))))
  type-error)

(deftest nthcdr.error.4
  (catch-type-error (nthcdr #\A (copy-tree '(a b c d))))
  type-error)

(deftest nthcdr.error.5
  (catch-type-error (nthcdr '(a) (copy-tree '(a b c d))))
  type-error)

(deftest nthcdr.error.6
  (catch-type-error (nthcdr -10 (copy-tree '(a b c d))))
  type-error)

(deftest nthcdr.error.7
  (classify-error (nthcdr))
  program-error)

(deftest nthcdr.error.8
  (classify-error (nthcdr 0))
  program-error)

(deftest nthcdr.error.9
  (classify-error (nthcdr 0 nil nil))
  program-error)

(deftest nthcdr.error.10
  (catch-type-error (nthcdr 3 (cons 'a 'b)))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rest

(deftest rest-1
    (rest (list 'a 'b 'c))
  (b c))

(deftest rest.error.1
  (classify-error (rest))
  program-error)

(deftest rest.error.2
  (classify-error (rest nil nil))
  program-error)
