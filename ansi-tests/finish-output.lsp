;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Jan 28 06:38:20 2004
;;;; Contains: Tests of FINISH-OUTPUT

(in-package :cl-test)

(deftest finish-output.1
  (finish-output)
  nil)

(deftest finish-output.2
  (finish-output t)
  nil)

(deftest finish-output.3
  (finish-output nil)
  nil)

(deftest finish-output.4
  (loop for s in (list *debug-io* *error-output* *query-io*
		       *standard-output* *trace-output* *terminal-io*)
	for results = (multiple-value-list (finish-output s))
	unless (equal results '(nil))
	collect s)
  nil)

;;; Error tests

(deftest finish-output.error.1
  (signals-error (finish-output nil nil) program-error)
  t)

(deftest finish-output.error.2
  (signals-error (finish-output t nil) program-error)
  t)

(deftest finish-output.error.3
  (loop for x in *mini-universe*
	unless (or (member x '(nil t))
		   (typep x 'stream)
		   (equalt
		    (eval `(multiple-value-list
			    (signals-error (finish-output ',x) type-error)))
		    '(t)))
	collect x)
  nil)
