;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Jan 29 21:28:25 2004
;;;; Contains: Tests of MAKE-BROADCAST-STREAM

(in-package :cl-test)

(deftest make-broadcast-stream.1
  (let ((s (make-broadcast-stream)))
    (values
     (notnot (typep s 'stream))
     (notnot (typep s 'broadcast-stream))
     (notnot (output-stream-p s))
     (progn (write-char #\x s) nil)
     ))
  t t t nil)

(deftest make-broadcast-stream.2
  (with-output-to-string
    (s1)
    (let ((s (make-broadcast-stream s1)))
      (assert (typep s 'stream))
      (assert (typep s 'broadcast-stream))
      (assert (output-stream-p s))
      (write-char #\x s)))
  "x")

(deftest make-broadcast-stream.3
  (let ((s1 (make-string-output-stream))
	(s2 (make-string-output-stream)))
    (let ((s (make-broadcast-stream s1 s2)))
      (format s "This is a test"))
    (values
     (get-output-stream-string s1)
     (get-output-stream-string s2)))
  "This is a test"
  "This is a test")

(deftest make-broadcast-stream.error.1
  (loop for x in *mini-universe*
	unless (or (and (typep x 'stream) (output-stream-p x))
		   (handler-case (progn (make-broadcast-stream x) nil)
				 (type-error () t)
				 (condition nil)))

	collect x)
  nil)

(deftest make-broadcast-stream.error.2
  (loop for x in *streams*
	unless (or (and (typep x 'stream) (output-stream-p x))
		   (handler-case (progn (make-broadcast-stream x) nil)
				 (type-error () t)
				 (condition nil)))

	collect x)
  nil)

