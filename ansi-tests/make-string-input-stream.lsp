;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Feb 14 18:36:48 2004
;;;; Contains: Tests for MAKE-STRING-INPUT-STREAM

(in-package :cl-test)

(deftest make-string-input-stream.1
  (let ((s (make-string-input-stream "")))
    (values
     (notnot (typep s 'stream))
     (notnot (streamp s))
     (notnot (input-stream-p s))
     (output-stream-p s)))
  t t t nil)

(deftest make-string-input-stream.2
  (let ((s (make-string-input-stream "abcd")))
    (values
     (notnot (typep s 'stream))
     (notnot (streamp s))
     (notnot (input-stream-p s))
     (output-stream-p s)))
  t t t nil)


(deftest make-string-input-stream.3
  (let ((s (make-string-input-stream "abcd" 1)))
    (values (read-line s)))
  "bcd")


(deftest make-string-input-stream.4
  (let ((s (make-string-input-stream "abcd" 0 2)))
    (values (read-line s)))
  "ab")

(deftest make-string-input-stream.5
  (let ((s (make-string-input-stream "abcd" 1 nil)))
    (values (read-line s)))
  "bcd")

(deftest  make-string-input-stream.6
  (let ((str1 (make-array 6 :element-type 'character
			  :initial-contents "abcdef"
			  :fill-pointer 4)))
    (let ((s (make-string-input-stream str1)))
      (values (read-line s) (read-char s nil :eof))))
  "abcd" :eof)









