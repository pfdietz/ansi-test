;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Oct  6 19:47:16 2002
;;;; Contains: Tests for CONSTANTLY

(in-package :cl-test)

(deftest constantly.1
  (let ((fn (cl::constantly 10))
	(x nil))
    (loop for i from 0 to (min 256 (1- call-arguments-limit))
	  always (prog1 (eql (apply fn x) 10)
		   (push 'a x))))
  t)
