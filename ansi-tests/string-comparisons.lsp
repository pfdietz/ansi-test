;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct  4 06:32:41 2002
;;;; Contains: Tests of string comparison functions

(in-package :cl-test)

(deftest string=.1
  (not (string= "abc" (copy-seq "abc")))
  nil)

(deftest string=.2
  (string= "A" "a")
  nil)

(deftest string=.3
  (not (string= #\a "a"))
  nil)

(deftest string=.4
  (not (string= '|abc| (copy-seq "abc")))
  nil)

(deftest string=.5
  (not (string= (copy-seq "abc") '#:|abc|))
  nil)

;;; Test that it doesn't stop at null characters
(deftest string=.6
  (let ((s1 (copy-seq "abc"))
	(s2 (copy-seq "abd"))
	(c (or (code-char 0) #\a)))
    (setf (char s1 1) c)
    (setf (char s2 1) c)
    (values (length s1) (length s2) (string= s1 s2)))
  3 3 nil)

(deftest string=.7
  (loop for i from 0 to 3
	collect (not (string= "abc" "abd" :start1 0 :end1 i :end2 i)))
  (nil nil nil t))

(deftest string=.8
  (loop for i from 0 to 3
	collect (not (string= "abc" "ab" :end1 i)))
  (t t nil t))

(deftest string=.9
  (loop for i from 0 to 3
	collect (not (string= "abc" "abd" :start2 0 :end2 i :end1 i)))
  (nil nil nil t))

(deftest string=.10
  (loop for i from 0 to 3
	collect (not (string= "ab" "abc" :end2 i)))
  (t t nil t))

(deftest string=.11
  (loop for i from 0 to 3
	collect (not (string= "xyab" "ab" :start1 i)))
  (t t nil t))

(deftest string=.12
  (loop for i from 0 to 3
	collect (not (string= "ab" "xyab" :start2 i)))
  (t t nil t))

(deftest string=.13
  (loop for i from 0 to 3
	collect (not (string= "xyab" "ab" :start1 i :end1 nil)))
  (t t nil t))

(deftest string=.14
  (loop for i from 0 to 3
	collect (not (string= "ab" "xyab" :start2 i :end2 nil)))
  (t t nil t))

;;; Random tests (of all the string comparson functions)

(deftest random-string-comparison-tests
  (loop for cmp in '(= /= < > <= >=)
	append
	(loop for case in '(nil t)
	      collect
	      (list cmp case
		    (random-string-compare-test 10 cmp case 1000))))
  ((= nil 0) (= t 0) (/= nil 0) (/= t 0) (< nil 0) (< t 0)
   (> nil 0) (> t 0) (<= nil 0) (<= t 0) (>= nil 0) (>= t 0)))








  



