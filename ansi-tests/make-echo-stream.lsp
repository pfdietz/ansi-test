;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Feb 12 04:34:42 2004
;;;; Contains: Tests of MAKE-ECHO-STREAM

(in-package :cl-test)

(deftest make-echo-stream.1
  (let* ((is (make-string-input-stream "foo"))
	 (os (make-string-output-stream))
	 (s (make-echo-stream is os)))
    (values
     (read-char s)
     (get-output-stream-string os)))
  #\f "f")

(deftest make-echo-stream.2
  (let* ((is (make-string-input-stream "foo"))
	 (os (make-string-output-stream))
	 (s (make-echo-stream is os)))
    (get-output-stream-string os))
  "")

(deftest make-echo-stream.3
  (let* ((is (make-string-input-stream "foo"))
	 (os (make-string-output-stream))
	 (s (make-echo-stream is os)))
    (values (read-line s nil)
	    (get-output-stream-string os)))
  "foo" "foo")

;;; More tests of stream functions on echo streams go here

;;; Error tests

(deftest make-echo-stream.error.1
  (signals-error (make-echo-stream) program-error)
  t)

(deftest make-echo-stream.error.2
  (signals-error (make-echo-stream *standard-input*) program-error)
  t)

(deftest make-echo-stream.error.3
  (signals-error (make-echo-stream *standard-input* *standard-output* nil)
		 program-error)
  t)




