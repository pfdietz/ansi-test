;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Apr 19 07:28:40 2004
;;;; Contains: Tests of printing of conses

(compile-and-load "printer-aux.lsp")

(in-package :cl-test)

(deftest print.cons.1
  (with-standard-io-syntax
   (write-to-string '(|A|) :case :upcase :pretty nil :escape nil :readably nil))
  "(A)")

(deftest print.cons.2
  (with-standard-io-syntax
   (write-to-string '(|A| |B|) :case :upcase :pretty nil :escape nil :readably nil))
  "(A B)")

(deftest print.cons.3
  (with-standard-io-syntax
   (write-to-string (cons '|A| '|B|) :case :upcase :pretty nil :escape nil :readably nil))
  "(A . B)")

(deftest print.cons.4
  (with-standard-io-syntax
   (write-to-string (let ((s '#:|X|)) (cons s s)) :case :upcase :pretty nil :escape t :readably nil))
  "(#:X . #:X)")

(deftest print.cons.5
  (with-standard-io-syntax
   (write-to-string (let ((s '#:|X|)) (cons s s)) :case :upcase :pretty nil :escape t :circle t :readably nil))
  "(#1=#:X . #1#)")

(deftest print.cons.6
  (with-standard-io-syntax
   (write-to-string (let ((s1 (make-symbol "X"))
			  (s2 (make-symbol "X")))
		      (list s1 s2 s1 s2))
		    :case :upcase :pretty nil :escape t :circle t :readably nil))
  "(#1=#:X #2=#:X #1# #2#)")

(deftest print.cons.7
  (with-standard-io-syntax
   (write-to-string (let ((a (list 17 nil)))
		      (setf (cdr a) a)
		      a)
		    :circle t :pretty nil :escape nil :readably nil))
  "#1=(17 . #1#)")

;;; Random printing

(deftest print.cons.random.1
  (trim-list
   (loop
    for x = (make-random-cons-tree (random 100))
    repeat 500
    nconc (randomly-check-readability x))
   10)
  nil)

;; random circular cons graphs
(deftest print.cons.random.2
  (loop repeat 50
	nconc
	(let* ((n 50)
	       (conses (apply #'vector
			      (loop repeat n collect (cons nil nil)))))
	  (loop for x across conses
		for j = (random n)
		for k = (random n)
		do (setf (car x) (elt conses j)
			 (cdr x) (elt conses k)))
	  (randomly-check-readability (elt conses 0) :test #'is-similar
				      :circle t)))
  nil)

;;; Printing with *print-length*

(deftest print.cons.length.1
  (with-standard-io-syntax
   (write-to-string '(a) :length 0 :readably nil :pretty nil :escape nil))
  "(...)")

(deftest print.cons.length.2
  (with-standard-io-syntax
   (write-to-string '(81) :length 1 :readably nil :pretty nil :escape nil))
  "(81)")

(deftest print.cons.length.3
  (with-standard-io-syntax
   (write-to-string '(4 . 8) :length 1 :readably nil :pretty nil :escape nil))
  "(4 . 8)")

(deftest print.cons.length.4
  (with-standard-io-syntax
   (write-to-string '(4 8) :length 1 :readably nil :pretty nil :escape nil))
  "(4 ...)")

(deftest print.cons.length.5
  (with-standard-io-syntax
   (write-to-string '(a b c d e f g h i j k l m n o p)
		    :case :downcase :length 10
		    :readably nil :pretty nil :escape nil))
  "(a b c d e f g h i j ...)")


(deftest print.cons.length.6
  (with-standard-io-syntax
   (write-to-string '(((((((0)))))))
		    :case :downcase :length 3
		    :readably nil :pretty nil :escape nil))
  "(((((((0)))))))")

;;; Printing with *print-level*

(deftest print.cons.level.1
  (with-standard-io-syntax
   (write-to-string '(a)
		    :case :downcase :level 0
		    :readably nil :escape nil
		    :pretty nil))
  "#")

(deftest print.cons.level.2
  (with-standard-io-syntax
   (write-to-string '(a)
		    :case :downcase :level 1
		    :readably nil :escape nil
		    :pretty nil))
  "(a)")

(deftest print.cons.level.3
  (with-standard-io-syntax
   (write-to-string '((a))
		    :case :downcase :level 1
		    :readably nil :escape nil
		    :pretty nil))
  "(#)")


(deftest print.cons.level.4
  (with-standard-io-syntax
   (write-to-string '(a)
		    :case :downcase :level 2
		    :readably nil :escape nil
		    :pretty nil))
  "(a)")

(deftest print.cons.level.5
  (with-standard-io-syntax
   (write-to-string '(#(a) #*1101 "abc")
		    :case :downcase :level 1
		    :readably nil :pretty nil))
  "(# #*1101 \"abc\")")


