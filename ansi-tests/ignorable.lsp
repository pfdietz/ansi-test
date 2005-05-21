;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 21 08:16:27 2005
;;;; Contains: Tests of the IGNORABLE declaration

(in-package :cl-test)

(deftest ignorable.1
  (let ((x 'foo)) (declare (ignorable x)))
  nil)

(deftest ignorable.2
  (let ((x 'foo)) (declare (ignorable x)) x)
  foo)

(deftest ignorable.3
  (flet ((%f () 'foo))
    (declare (ignorable (function %f))))
  nil)
    
(deftest ignorable.4
  (flet ((%f () 'foo))
    (declare (ignorable (function %f)))
    (%f))
  foo)
