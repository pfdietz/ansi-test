;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jan 19 06:55:04 2004
;;;; Contains: Tests of READ-SEQUENCE

(in-package :cl-test)

(deftest read-sequence.1
  (let ((s (make-string 5)))
    (with-input-from-string
     (is "abcdefghijk")
     (values
      (read-sequence s is)
      s)))
  5
  "abcde")

(deftest read-sequence.2
  (let ((s (make-string 5 :initial-element #\Space)))
    (with-input-from-string
     (is "abc")
     (values
      (read-sequence s is)
      s)))
  3
  "abc  ")

(deftest read-sequence.3
  (let ((s (make-string 5 :initial-element #\Space)))
    (with-input-from-string
     (is "abcdefghijk")
     (values
      (read-sequence s is :start 1)
      s)))
  5
  " abcd")

(deftest read-sequence.4
  (let ((s (make-string 5 :initial-element #\Space)))
    (with-input-from-string
     (is "abcdefghijk")
     (values
      (read-sequence s is :end 3)
      s)))
  3
  "abc  ")

(deftest read-sequence.5
  (let ((s (make-string 5 :initial-element #\Space)))
    (with-input-from-string
     (is "abcdefghijk")
     (values
      (read-sequence s is :start 1 :end 4)
      s)))
  4
  " abc ")








