;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Jan 28 06:43:17 2004
;;;; Contains: Tests of CLEAR-OUTPUT

(in-package :cl-test)

(deftest clear-output.1
  (progn (finish-output) (clear-output))
  nil)

(deftest clear-output.2
  (progn (finish-output) (clear-output t))
  nil)

(deftest clear-output.3
  (progn (finish-output) (clear-output nil))
  nil)

(deftest clear-output.4
  (loop for s in (list *debug-io* *error-output* *query-io*
		       *standard-output* *trace-output* *terminal-io*)
	for dummy = (finish-output s)
	for results = (multiple-value-list (clear-output s))
	unless (equal results '(nil))
	collect s)
  nil)

;;; Error tests

(deftest clear-output.error.1
  (signals-error (clear-output nil nil) program-error)
  t)

(deftest clear-output.error.2
  (signals-error (clear-output t nil) program-error)
  t)

(deftest clear-output.error.3
  (loop for x in *mini-universe*
	unless (or (member x '(nil t))
		   (typep x 'stream)
		   (equalt
		    (eval `(multiple-value-list
			    (signals-error (clear-output ',x) type-error)))
		    '(t)))
	collect x)
  nil)

