;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Apr 19 07:28:40 2004
;;;; Contains: Tests of printing of conses

(in-package :cl-test)

(deftest print.cons.1
  (with-standard-io-syntax
   (write-to-string '(a) :case :upcase :pretty nil :escape nil :readably nil))
  "(A)")

(deftest print.cons.2
  (with-standard-io-syntax
   (write-to-string '(a b) :case :upcase :pretty nil :escape nil :readably nil))
  "(A B)")

(deftest print.cons.3
  (with-standard-io-syntax
   (write-to-string (cons 'a 'b) :case :upcase :pretty nil :escape nil :readably nil))
  "(A . B)")

(deftest print.cons.4
  (with-standard-io-syntax
   (write-to-string (let ((s '#:x)) (cons s s)) :case :upcase :pretty nil :escape t :readably nil))
  "(#:X . #:X)")

(deftest print.cons.5
  (with-standard-io-syntax
   (write-to-string (let ((s '#:x)) (cons s s)) :case :upcase :pretty nil :escape t :circle t :readably nil))
  "(#1=#:X . #1#)")

(deftest print.cons.6
  (with-standard-io-syntax
   (write-to-string (let ((s1 (make-symbol "X"))
			  (s2 (make-symbol "X")))
		      (list s1 s2 s1 s2))
		    :case :upcase :pretty nil :escape t :circle t :readably nil))
  "(#1=#:X #2=#:X #1# #2#)")







