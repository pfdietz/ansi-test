;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Feb 14 08:41:18 2004
;;;; Contains: Tests of MAKE-CONCATENATED-STREAM

(in-package :cl-test)

(deftest make-concatenated-stream.1
  (let ((s (make-concatenated-stream)))
    (read s nil :eof))
  :eof)

(deftest make-concatenated-stream.2
  (let ((s (make-concatenated-stream)))
    (notnot-mv (input-stream-p s)))
  t)

(deftest make-concatenated-stream.3
  (let ((s (make-concatenated-stream)))
    (output-stream-p s))
  nil)

