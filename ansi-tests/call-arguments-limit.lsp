;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Oct 10 22:39:25 2002
;;;; Contains: Tests for CALL-ARGUMENTS-LIMIT

(in-package :cl-test)

(deftest call-arguments-limit.1
  (not (constantp 'call-arguments-limit))
  nil)

(deftest call-arguments-limit.2
  (not (typep call-arguments-limit 'integer))
  nil)

(deftest call-arguments-limit.3
  (< call-arguments-limit 50)
  nil)

(deftest call-arguments-limit.4
  (let* ((m (min 65536 (1- call-arguments-limit)))
	 (args (make-list m :initial-element 'a)))
    (equal (apply #'list args) args))
  t)

(deftest call-arguments-limit.5
  (< call-arguments-limit lambda-parameters-limit)
  nil)

