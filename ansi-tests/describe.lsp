;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Dec 12 13:22:13 2004
;;;; Contains: Tests of DESCRIBE

(in-package :cl-test)

(deftest describe.1
  (loop for x in *universe*
	for s1 = nil
	for s2 = nil
	do
	(with-open-stream
	 (*standard-output* (make-string-output-stream))
	 (with-open-stream
	  (tio-input (make-string-input-stream "X"))
	  (with-open-stream
	   (tio-output (make-string-output-stream))
	   (with-open-stream
	    (*terminal-io* (make-two-way-stream tio-input tio-output))
	    (let ((*print-circle* t)
		  (*print-readably* nil))
	      (assert (null (multiple-value-list (describe x))))))
	   (setq s2 (get-output-stream-string tio-output)))
	  (assert (equal (read-char tio-input) #\X)))
	 (setq s1 (get-output-stream-string *standard-output*)))
	when (and (equal s1 "") (equal s2 ""))
	collect x)
  nil)

(deftest describe.2
  (loop for x in *universe*
	for s1 = nil
	for s2 = nil
	for s3 =
	(with-output-to-string
	  (s)
	  (with-open-stream
	   (*standard-output* (make-string-output-stream))
	   (with-open-stream
	    (tio-input (make-string-input-stream "X"))
	    (with-open-stream
	     (tio-output (make-string-output-stream))
	     (with-open-stream
	      (*terminal-io* (make-two-way-stream tio-input tio-output))
	      (let ((*print-circle* t)
		    (*print-readably* nil))
		(assert (null (multiple-value-list (describe x s))))))
	     (setq s2 (get-output-stream-string tio-output)))
	    (assert (equal (read-char tio-input) #\X)))
	   (setq s1 (get-output-stream-string *standard-output*))))
	when (or (equal s3 "") (not (equal "" s2)) (not (equal "" s1)))
	collect (list x s1 s2 s3))
  nil)

(deftest describe.3
  (loop for x in *universe*
	for s1 = nil
	for s2 = nil
	do
	(with-open-stream
	 (*standard-output* (make-string-output-stream))
	 (with-open-stream
	  (tio-input (make-string-input-stream "X"))
	  (with-open-stream
	   (tio-output (make-string-output-stream))
	   (with-open-stream
	    (*terminal-io* (make-two-way-stream tio-input tio-output))
	    (let ((*print-circle* t)
		  (*print-readably* nil))
	      (assert (null (multiple-value-list (describe x t))))))
	   (setq s2 (get-output-stream-string tio-output)))
	  (assert (equal (read-char tio-input) #\X)))
	 (setq s1 (get-output-stream-string *standard-output*)))
	when (or (equal "" s2) (not (equal "" s1)))
	collect (list x s1 s2))
  nil)

(deftest describe.4
  (loop for x in *universe*
	for s1 = nil
	for s2 = nil
	do
	(with-open-stream
	 (*standard-output* (make-string-output-stream))
	 (with-open-stream
	  (tio-input (make-string-input-stream "X"))
	  (with-open-stream
	   (tio-output (make-string-output-stream))
	   (with-open-stream
	    (*terminal-io* (make-two-way-stream tio-input tio-output))
	    (let ((*print-circle* t)
		  (*print-readably* nil))
	      (assert (null (multiple-value-list (describe x nil))))))
	   (setq s2 (get-output-stream-string tio-output)))
	  (assert (equal (read-char tio-input) #\X)))
	 (setq s1 (get-output-stream-string *standard-output*)))
	when (or (equal "" s1) (not (equal "" s2)))
	collect (list x s1 s2))
  nil)

;;; Error cases

(deftest describe.error.1
  (signals-error (describe) program-error)
  t)

(deftest describe.error.2
  (signals-error (with-output-to-string (s) (describe nil s nil)) program-error)
  t)

