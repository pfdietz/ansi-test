;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 19 21:55:19 2003
;;;; Contains: Tests of COPY-LIST

(in-package :cl-test)

(deftest copy-list.1
  (check-copy-list '(a b c d))
  (a b c d))

;; Check that copy-list works on dotted lists

(deftest copy-list.2
  (check-copy-list '(a . b))
 (a . b))

(deftest copy-list.3
  (check-copy-list '(a b c . d))
  (a b c . d))

(deftest copy-list.4
  (let ((i 0))
    (values (copy-list (progn (incf i) '(a b c)))
	    i))
  (a b c) 1)

(deftest copy-list.error.1
  (classify-error (copy-list))
  program-error)

(deftest copy-list.error.2
  (classify-error (copy-list nil nil))
  program-error)
