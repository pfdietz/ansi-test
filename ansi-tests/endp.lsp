;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 28 07:34:40 1998
;;;; Contains: Tests of ENDP

(in-package :cl-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; endp

(deftest endp-nil
  (notnot-mv (endp nil))
  t)

(deftest endp-cons
  (endp (cons 'a 'a))
  nil)

(deftest endp-singleton-list
  (endp '(a))
  nil)

(deftest endp.order.1
  (let ((i 0))
    (values
     (endp (progn (incf i) '(a b c)))
     i))
  nil 1)

(deftest endp-symbol-error
  (catch-type-error (endp 'a))
  type-error)

(deftest endp-fixnum-error
  (catch-type-error (endp 1))
  type-error)

(deftest endp-float-error
  (catch-type-error (endp 0.9212d4))
  type-error)

(deftest endp.error.4
  (classify-error (endp))
  program-error)

(deftest endp.error.5
  (classify-error (endp nil nil))
  program-error)

(deftest endp.error.6
  (catch-type-error (locally (endp 1)))
  type-error)
