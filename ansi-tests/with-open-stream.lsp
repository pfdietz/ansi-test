;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Dec 13 01:42:59 2004
;;;; Contains: Tests of WITH-OPEN-STREAM

(in-package :cl-test)

(deftest with-open-stream.1
  (with-open-stream (os (make-string-output-stream)))
  nil)

(deftest with-open-stream.2
  (with-open-stream (os (make-string-output-stream))
		    (declare (ignore os)))
  nil)

(deftest with-open-stream.3
  (with-open-stream (os (make-string-output-stream))
		    (declare (ignore os))
		    (declare (type string-stream os)))
  nil)

(deftest with-open-stream.4
  (with-open-stream (os (make-string-output-stream))
		    (declare (ignore os))
		    (values)))

(deftest with-open-stream.5
  (with-open-stream (os (make-string-output-stream))
		    (declare (ignore os))
		    (values 'a 'b))
  a b)
