;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Apr  7 07:24:43 2003
;;;; Contains: Auxiliary functions for number tests

(in-package :cl-test)

(defun =.4-fn ()
  (loop for x in *numbers*
	append
	(loop for y in *numbers*
	      unless (if (= x y) (= y x) (not (= y x)))
	      collect (list x y))))
