;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jan  2 08:12:51 2005
;;;; Contains: Tests of standard syntax

(in-package :cl-test)

(defmacro def-syntax-test (name form &rest expected-results)
  `(deftest ,name
     (with-standard-io-syntax ,form)
     ,@expected-results))

;;; Test that 
(def-syntax-test syntax.whitespace.1
  ;; Check that various standard or semistandard characters are whitespace[2]
  (let ((names '("Tab" "Newline" "Linefeed" "Space" "Return" "Page")))
    (loop for name in names
	  for c = (name-char name)
	  nconc
	  (when c
	    (let* ((s (concatenate 'string (string c) "123"))
		   (val (read-from-string s)))
	      (unless (eql val 123)
		(list (list name c s val)))))))
  nil)

(def-syntax-test syntax.constituent.1
  ;; Tests of various characters that they are constituent characters,
  ;; and parse to symbols
  (let ((chars (concatenate
		'string
		"!$%&*<=>?@[]^_-{}+/"
		"abcdefghijklmnopqrstuvwxyz"
		"ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
    (loop for c across chars
	  for s = (string c)
	  for sym = (read-from-string s)
	  unless (string= (symbol-name sym) (string-upcase s))
	  collect (list c sym)))
  nil)

;;; Backspace is an invalid constituent character

(def-syntax-test syntax.backspace.invalid
  (let ((c (name-char "Backspace")))
    (if (not c) t
      (eval `(signals-error (read-from-string (string ,c)) reader-error))))
  t)

;;; Rubout is an invalid constituent character

(def-syntax-test syntax.rubout.invalid
  (let ((c (name-char "Rubout")))
    (if (not c) t
      (eval `(signals-error (read-from-string (string ,c)) reader-error))))
  t)
