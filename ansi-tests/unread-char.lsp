;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jan 18 20:05:36 2004
;;;; Contains: Tests of UNREAD-CHAR

(in-package :cl-test)

(deftest unread-char.1
  (with-input-from-string
   (*standard-input* "abc")
   (values
    (read-char)
    (unread-char #\a)
    (read-char)
    (read-char)
    (unread-char #\b)
    (read-char)
    (read-char)))
  #\a nil #\a #\b nil #\b #\c)

(deftest unread-char.2
  (with-input-from-string
   (s "abc")
   (values
    (read-char s)
    (unread-char #\a s)
    (read-char s)
    (read-char s)
    (unread-char #\b s)
    (read-char s)
    (read-char s)))
  #\a nil #\a #\b nil #\b #\c)

(deftest unread-char.3
  (with-input-from-string
   (is "abc")
   (with-output-to-string
     (os)
     (let ((s (make-echo-stream is os)))
       (read-char s)
       (unread-char #\a s)
       (read-char s)
       (read-char s)
       (read-char s)
       (unread-char #\c s)
       (read-char s))))
  "abc")

;;; Error tests

(deftest unread-char.error.1
  (signals-error (unread-char) program-error)
  t)

(deftest unread-char.error.2
  (signals-error
   (with-input-from-string
    (s "abc")
    (read-char s)
    (unread-char #\a s nil))
   program-error)
  t)


