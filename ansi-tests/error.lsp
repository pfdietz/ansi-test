;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan 28 21:37:43 2003
;;;; Contains: Tests of ERROR

(in-package :cl-test)

(deftest error.1
  (let ((fmt "Error"))
    (handler-case
     (error fmt)
     (simple-error (c) (frob-simple-error c fmt))))
  t)

(deftest error.2
  (let* ((fmt "Error")
	 (cnd (make-condition 'simple-error :format-control fmt)))
    (handler-case
     (error cnd)
     (simple-error (c) (frob-simple-error c fmt))))
  t)

(deftest error.3
  (let ((fmt "Error"))
    (handler-case
     (error 'simple-error :format-control fmt)
     (simple-error (c) (frob-simple-error c fmt))))
  t)

(deftest error.4
  (let ((fmt "Error: ~A"))
    (handler-case
     (error fmt 10)
     (simple-error (c) (frob-simple-error c fmt 10))))
  t)

(deftest error.5
  (let ((fmt (formatter "Error")))
    (handler-case
     (error fmt)
     (simple-error (c) (frob-simple-error c fmt))))
  t)
