;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Aug 29 17:31:24 2004
;;;; Contains: Tests of SIMPLE-STRING-P

(in-package :cl-test)

(deftest simple-string-p.1
  (loop for x in *universe*
	always (if (typep x 'simple-string)
		   (simple-string-p x)
		 (not (simple-string-p x))))
  t)

(deftest simple-string-p.2
  (notnot-mv (simple-string-p "ancd"))
  t)

(deftest simple-string-p.3
  (simple-string-p 0)
  nil)

(deftest simple-string-p.4
  (simple-string-p (make-array 4 :element-type 'character
			       :initial-contents '(#\a #\a #\a #\b)
			       :fill-pointer t))
  nil)

(deftest simple-string-p.5
  (notnot-mv
   (simple-string-p (make-array
		     4 :element-type 'base-char
		     :initial-contents '(#\a #\a #\a #\b))))
  t)

(deftest simple-string-p.6
  (notnot-mv
   (simple-string-p (make-array
		     4 :element-type 'standard-char
		     :initial-contents '(#\a #\a #\a #\b))))
  t)

(deftest simple-string-p.7
  (let* ((s (make-array 10 :element-type 'character
			:initial-element #\a))
	 (s2 (make-array 4 :element-type 'character
			 :displaced-to s
			 :displaced-index-offset 2)))
    (simple-string-p s2))
  nil)

(deftest simple-string-p.8
  :notes (:nil-vectors-are-strings)
  (notnot-mv (simple-string-p (make-array '(0) :element-type nil)))
  t)

(deftest simple-string-p.9
  :notes (:nil-vectors-are-strings)
  (notnot-mv (simple-string-p (make-array '(37) :element-type nil)))
  t)
