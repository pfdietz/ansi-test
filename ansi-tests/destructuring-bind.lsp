;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Oct 10 23:25:50 2002
;;;; Contains: Tests for DESTRUCTURING-BIND

(in-package :cl-test)

;;; See the page for this in section 5.3
;;; Also, see destructuring lambda lists in section 3.4.5

(deftest destructuring-bind.1
  (destructuring-bind (x y z) '(a b c) (values x y z))
  a b c)

(deftest destructuring-bind.2
  (destructuring-bind (x y &rest z) '(a b c d) (values x y z))
  a b (c d))

(deftest destructuring-bind.3
  (destructuring-bind (x y &optional z) '(a b c) (values x y z))
  a b c)

(deftest destructuring-bind.4
  (destructuring-bind (x y &optional z) '(a b) (values x y z))
  a b nil)

(deftest destructuring-bind.5
  (destructuring-bind (x y &optional (z 'w)) '(a b) (values x y z))
  a b w)

(deftest destructuring-bind.6
  (destructuring-bind (x y &optional (z 'w z-p)) '(a b) (values x y z z-p))
  a b w nil)

(deftest destructuring-bind.7
  (destructuring-bind (x y &optional (z 'w z-p)) '(a b c) (values x y z z-p))
  a b c t)

(deftest destructuring-bind.8
  (destructuring-bind (x y &optional z w) '(a b c) (values x y z w))
  a b c nil)

(deftest destructuring-bind.9
  (destructuring-bind ((x y)) '((a b)) (values x y))
  a b)

(deftest destructuring-bind.10
  (destructuring-bind (&whole w (x y)) '((a b)) (values x y w))
  a b ((a b)))

(deftest destructuring-bind.11
  (destructuring-bind ((x . y) . w) '((a b) c) (values x y w))
  a (b) (c))

(deftest destructuring-bind.12
  (destructuring-bind (x y &body z) '(a b c d) (values x y z))
  a b (c d))

(deftest destructuring-bind.13
  (destructuring-bind (&whole x y z) '(a b) (values x y z))
  (a b) a b)

(deftest destructuring-bind.14
  (destructuring-bind (w (&whole x y z)) '(1 (a b)) (values w x y z))
  1 (a b) a b)

(deftest destructuring-bind.15
  (destructuring-bind (&key a b c) '(:a 1) (values a b c))
  1 nil nil)

(deftest destructuring-bind.16
  (destructuring-bind (&key a b c) '(:b 1) (values a b c))
  nil 1 nil)

(deftest destructuring-bind.17
  (destructuring-bind (&key a b c) '(:c 1) (values a b c))
  nil nil 1)

(deftest destructuring-bind.18
  (destructuring-bind ((&key a b c)) '((:c 1 :b 2)) (values a b c))
  nil 2 1)

;;; Error cases

#|
(deftest destructuring-bind.error.1
  (classify-error (destructuring-bind (a b c) nil (list a b c)))
  program-error)

(deftest destructuring-bind.error.2
  (classify-error (destructuring-bind ((a b c)) nil (list a b c)))
  program-error)

(deftest destructuring-bind.error.3
  (classify-error (destructuring-bind (a b) 'x (list a b)))
  program-error)

(deftest destructuring-bind.error.4
  (classify-error (destructuring-bind (a . b) 'x (list a b)))
  program-error)
|#

;;; (deftest destructuring-bind.error.5
;;;  (classify-error (destructuring-bind))
;;;  program-error)
;;;
;;; (deftest destructuring-bind.error.6
;;;  (classify-error (destructuring-bind x))
;;;  program-error)

