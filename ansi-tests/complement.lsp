;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Oct  6 20:04:33 2002
;;;; Contains: Tests for COMPLEMENT

(in-package :cl-test)

(deftest complement.1
  (not (funcall (cl::complement #'identity) nil))
  nil)

(deftest complement.2
  (funcall (cl::complement #'identity) t)
  nil)

(deftest complement.3
  (every #'(lambda (x) (eql (funcall (cl::complement #'not) x)
			    (not (not x))))
	 *universe*)
  t)

(deftest complement.4
  (let ((x '(#\b)))
    (loop for i from 2 to (min 256 (1- call-arguments-limit))
	  always (progn
		   (push #\a x)
		   (apply (cl::complement #'char=) x))))
  t)

(deftest complement.5
  (classify-error (complement))
  program-error)


(deftest complement.6
  (classify-error (complement #'not t))
  program-error)


