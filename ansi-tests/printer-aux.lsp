;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Feb 23 06:20:00 2004
;;;; Contains: Auxiliary functions and macros for printer tests

(in-package :cl-test)

(defmacro def-print-test (name form result &rest bindings)
  `(deftest ,name
     (equalt (let ,bindings
	       (with-standard-io-syntax (prin1 ,form)))
	     ,result)
     t))

