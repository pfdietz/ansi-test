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

(deftest read-sequence.6
  (let ((s (make-string 5 :initial-element #\Space)))
    (with-input-from-string
     (is "abcdefghijk")
     (values
      (read-sequence s is :start 0 :end 0)
      s)))
  0
  "     ")


;;; Error cases

(deftest read-sequence.error.1
  (signals-error (read-sequence) program-error)
  t)

(deftest read-sequence.error.2
  (signals-error (read-sequence (make-string 10)) program-error)
  t)

(deftest read-sequence.error.3
  (signals-error
   (read-sequence (make-string 5) (make-string-input-stream "abc") :start)
   program-error)
  t)

(deftest read-sequence.error.4
  (signals-error
   (read-sequence (make-string 5) (make-string-input-stream "abc") :foo 1)
   program-error)
  t)

(deftest read-sequence.error.5
  (signals-error
   (read-sequence (make-string 5) (make-string-input-stream "abc")
		  :allow-other-keys nil :bar 2)
   program-error)
  t)

(deftest read-sequence.error.6
  (signals-error
   (read-sequence 'a (make-string-input-stream "abc"))
   type-error)
  t)

(deftest read-sequence.error.7
  (signals-error
   (read-sequence (cons 'a 'b) (make-string-input-stream "abc"))
   type-error)
  t)

(deftest read-sequence.error.8
  (signals-error
   (read-sequence (make-string 3) (make-string-input-stream "abc")
		  :start -1)

   type-error)
  t)

(deftest read-sequence.error.9
  (signals-error
   (read-sequence (make-string 3) (make-string-input-stream "abc")
		  :start 'a)

   type-error)
  t)

(deftest read-sequence.error.10
  (signals-error
   (read-sequence (make-string 3) (make-string-input-stream "abc")
		  :end -1)
   type-error)
  t)

(deftest read-sequence.error.11
  (signals-error
   (read-sequence (make-string 3) (make-string-input-stream "abc")
		  :end 'b)
   type-error)
  t)



