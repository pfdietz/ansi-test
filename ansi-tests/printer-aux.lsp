;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Feb 23 06:20:00 2004
;;;; Contains: Auxiliary functions and macros for printer tests

(in-package :cl-test)

(defmacro def-print-test (name form result &rest bindings)
  `(deftest ,name
     (equalt
      (with-standard-io-syntax
       (let ,bindings
	 (with-output-to-string (*standard-output*) (prin1 ,form))))
      ,result)
     t))
