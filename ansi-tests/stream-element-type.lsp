;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan 13 20:09:50 2004
;;;; Contains: Tests for STREAM-ELEMENT-TYPE

(in-package :cl-test)

(deftest stream-element-type.1
  (loop for s in (list *debug-io* *error-output* *query-io*
		       *standard-input* *standard-output*
		       *trace-output* *terminal-io*)
	for results = (multiple-value-list (open-stream-p s))
	unless (and (eql (length results) 1)
		    (car results))
	collect s)
  nil)

(deftest stream-element-type.2
  (let ((pn "foo.txt"))
    (loop for i from 1 to 100
	  for etype = `(unsigned-byte ,i)
	  for s = (progn (when (probe-file pn) (delete-file pn))
			 (open pn :direction :output
			       :element-type etype))
	  unless
	  (multiple-value-bind (sub good)
	      (subtypep etype (stream-element-type s))
	    (close s)
	    (or sub (not good)))
	  collect i))
  nil)

(deftest stream-element-type.3
  (let ((pn "foo.txt"))
    (loop for i from 1 to 100
	  for etype = `(signed-byte ,i)
	  for s = (progn (when (probe-file pn) (delete-file pn))
			 (open pn :direction :output
			       :element-type etype))
	  unless
	  (multiple-value-bind (sub good)
	      (subtypep etype (stream-element-type s))
	    (close s)
	    (or sub (not good)))
	  collect i))
  nil)

(deftest stream-element-type.4
  (let ((pn "foo.txt"))
    (loop for i from 1 to 100
	  for etype = `(integer 0 ,i)
	  for s = (progn (when (probe-file pn) (delete-file pn))
			 (open pn :direction :output
			       :element-type etype))
	  unless
	  (multiple-value-bind (sub good)
	      (subtypep etype (stream-element-type s))
	    (close s)
	    (or sub (not good)))
	  collect i))
  nil)


(deftest stream-element-type.5
  (let ((pn "foo.txt"))
    (when (probe-file pn) (delete-file pn))
    (let ((s (open pn :direction :output)))
      (let ((etype (stream-element-type s)))
	(unwind-protect
	    (equalt (multiple-value-list (subtypep* 'character etype))
		    '(nil t))
	  (close s)))))
  nil)

(deftest stream-element-type.error.1
  (signals-error (stream-element-type) program-error)
  t)

(deftest stream-element-type.error.2
  (signals-error (stream-element-type *standard-input* nil) program-error)
  t)

(deftest stream-element-type.error.3
  (loop for x in *mini-universe*
	unless (or (typep x 'stream)
		   (eval `(signals-error (stream-element-type ',x)
					 type-error)))
	collect x)
  nil)
