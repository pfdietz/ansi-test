;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Jan 30 05:39:56 2004
;;;; Contains: Tests for MAKE-TWO-WAY-STREAM

(in-package :cl-test)

(deftest make-two-way-stream.1
  (let* ((is (make-string-input-stream "foo"))
	 (os (make-string-output-stream))
	 (s (make-two-way-stream is os)))
    (assert (typep s 'stream))
    (assert (typep s 'two-way-stream))
    (assert (streamp s))
    (assert (open-stream-p s))
    (assert (input-stream-p s))
    (assert (output-stream-p s))
    (assert (stream-element-type s))
    (values
     (read-char s)
     (write-char #\b s)
     (read-char s)
     (write-char #\a s)
     (read-char s)
     (write-char #\r s)
     (get-output-stream-string os)))
  #\f #\b #\o #\a #\o #\r "bar")

;;; FIXME
;;; Add tests for: close,
;;;  peek-char, read-char-no-hang, terpri, fresh-line, unread-char,
;;;  read-line, write-line, write-string, read-sequence, write-sequence,
;;;  read-byte, write-byte, listen, clear-input, finish-output, force-output,
;;;  clear-output, format, print, prin1, princ

;;; Error tests

(deftest make-two-way-stream.error.1
  (signals-error (make-two-way-stream) program-error)
  t)

(deftest make-two-way-stream.error.2
  (signals-error (make-two-way-stream (make-string-input-stream "foo"))
		 program-error)
  t)

(deftest make-two-way-stream.error.3
  (signals-error (let ((os (make-string-output-stream)))
		   (make-two-way-stream (make-string-input-stream "foo")
					os nil))
		 program-error)
  t)

(deftest make-two-way-stream.error.4
  (loop for x in *mini-universe*
	unless (or (and (typep x 'stream) (input-stream-p x))
		   (eval `(signals-error
			   (let ((os (make-string-output-stream)))
			     (make-two-way-stream ',x os))
			   type-error)))
	collect x)
  nil)

(deftest make-two-way-stream.error.5
  (loop for x in *streams*
	unless (or (and (typep x 'stream) (input-stream-p x))
		   (eval `(signals-error
			   (let ((os (make-string-output-stream)))
			     (make-two-way-stream ',x os))
			   type-error)))
	collect x)
  nil)

(deftest make-two-way-stream.error.6
  (loop for x in *mini-universe*
	unless (or (and (typep x 'stream) (output-stream-p x))
		   (eval `(signals-error
			   (let ((is (make-string-input-stream "foo")))
			     (make-two-way-stream is ',x))
			   type-error)))
	collect x)
  nil)

(deftest make-two-way-stream.error.7
  (loop for x in *streams*
	unless (or (and (typep x 'stream) (output-stream-p x))
		   (eval `(signals-error
			   (let ((is (make-string-input-stream "foo")))
			     (make-two-way-stream is ',x))
			   type-error)))
	collect x)
  nil)



						