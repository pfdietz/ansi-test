;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Oct  6 20:04:33 2002
;;;; Contains: Tests for COMPLEMENT

(in-package :cl-test)

(deftest complement.1
  (notnot-mv (funcall (complement #'identity) nil))
  t)

(deftest complement.2
  (funcall (complement #'identity) t)
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
		   (apply (complement #'char=) x))))
  t)

(deftest complement.5
  (notnot-mv (complement #'identity))
  t)

(deftest complement.order.1
  (let ((i 0))
    (let ((fn (complement (progn (incf i) #'null))))
      (values
       i
       (mapcar fn '(a b nil c 1 nil t nil))
       i)))
  1 (t t nil t t nil t nil) 1)

(deftest complement.error.1
  (classify-error (complement))
  program-error)

(deftest complement.error.2
  (classify-error (complement #'not t))
  program-error)

(deftest complement.error.3
  (classify-error (funcall (complement #'identity)))
  program-error)

(deftest complement.error.4
  (classify-error (funcall (complement #'identity) t t))
  program-error)
