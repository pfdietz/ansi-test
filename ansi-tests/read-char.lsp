;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jan 18 08:53:56 2004
;;;; Contains: Tests of READ-CHAR

(in-package :cl-test)

(deftest read-char.1
  (with-input-from-string
   (*standard-input* "a")
   (read-char))
  #\a)

(deftest read-char.2
  (with-input-from-string
   (*standard-input* "abc")
   (values
    (read-char)
    (read-char)
    (read-char)))
  #\a #\b #\c)

(when (code-char 0)
  (deftest read-char.3
    (with-input-from-string
     (*standard-input* (concatenate 'string
				    "a"
				    (string (code-char 0))
				    "b"))
     (values
      (read-char)
      (read-char)
      (read-char)))
    #\a #.(code-char 0) #\b))
