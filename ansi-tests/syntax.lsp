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

;;; Digits are alphabetic if >= the read base

(def-syntax-test syntax.digits.alphabetic.1
  (loop for base from 2 to 9
	nconc
	(let ((*read-base* base))
	  (loop for digit-val from base to 9
		for c = (elt "0123456789" digit-val)
		for s = (string c)
		for val = (read-from-string s)
		unless (and (symbolp val)
			    (string= s (symbol-name val)))
		collect (list base digit-val c s val))))
  nil)

;;; Reading escaped characters

(def-syntax-test syntax.escaped.1
  (loop for c across +standard-chars+
	for s0 = (string c)
	for s = (concatenate 'string "\\" s0)
	for sym = (read-from-string s)
	unless (and (symbolp sym)
		    (string= (symbol-name sym) s0))
	collect (list c s0 s sym))
  nil)

(def-syntax-test syntax.escaped.2
  (let ((count 0))
    (loop for i from 0 below (min 65536 char-code-limit)
	  for c = (code-char i)
	  for s0 = (and c (string c))
	  for s = (and c (concatenate 'string "\\" s0))
	  for sym = (and c (read-from-string s))
	  unless (and c
		      (symbolp sym)
		      (string= (symbol-name sym) s0))
	  collect (progn
		    (when (> (incf count) 100) (loop-finish))
		    (list i c s0 s sym))))
  nil)

(def-syntax-test syntax.escaped.3
  (loop for i = (random (min char-code-limit (ash 1 24)))
	for c = (code-char i)
	for s0 = (and c (string c))
	for s = (and c (concatenate 'string "\\" s0))
	for sym = (and c (read-from-string s))
	repeat 1000
	unless (and c
		    (symbolp sym)
		    (string= (symbol-name sym) s0))
	collect (list i c s0 s sym))
  nil)

(def-syntax-test syntax.escaped.4
  (loop for c across +standard-chars+
	for bad = (find c "\\|")
	for s0 = (string c)
	for s = (concatenate 'string "|" s0 "|")
	for sym = (and (not bad) (read-from-string s))
	unless (or bad
		   (and (symbolp sym)
			(string= (symbol-name sym) s0)))
	collect (list c s0 s sym))
  nil)

(def-syntax-test syntax.escaped.5
  (let ((count 0))
    (loop for i from 0 below (min 65536 char-code-limit)
	  for c = (code-char i)
	  for bad = (find c "\\|")
	  for s0 = (string c)
	  for s = (concatenate 'string "|" s0 "|")
	  for sym = (and (not bad) (read-from-string s))
	  unless (or bad
		     (and (symbolp sym)
			  (string= (symbol-name sym) s0)))
	  collect (progn
		    (when (> (incf count) 100) (loop-finish))
		    (list c s0 s sym))))
  nil)

(def-syntax-test syntax.escaped.6
  (loop for i = (random (min char-code-limit (ash 1 24)))
	for c = (code-char i)
	for bad = (find c "\\|")
	for s0 = (string c)
	for s = (concatenate 'string "|" s0 "|")
	for sym = (and (not bad) (read-from-string s))
	repeat 1000
	unless (or bad
		   (and (symbolp sym)
			(string= (symbol-name sym) s0)))
	collect (list c s0 s sym))
  nil)

(def-syntax-test syntax.escape.whitespace.1
  (let ((names '("Tab" "Newline" "Linefeed" "Space" "Return" "Page"
		 "Rubout" "Backspace")))
    (loop for name in names
	  for c = (name-char name)
	  nconc
	  (when c
	    (let* ((s (concatenate 'string "\\" (string c)))
		   (val (read-from-string s)))
	      (unless (eql val (intern (string c)))
		(list (list name c s val)))))))
  nil)

#|
(def-syntax-test syntax.escape.whitespace.2
  (let ((names '("Tab" "Newline" "Linefeed" "Space" "Return" "Page")))
    (loop for name in names
	  for c = (name-char name)
	  nconc
	  (when c
	    (let* ((s (concatenate 'string "|" (string c) "|"))
		   (val (read-from-string s)))
	      (unless (eql val (intern (string c)))
		(list (list name c s val)))))))
  nil)
|#

(def-syntax-test syntax.multiple-escape.invalid.backspace
  (let ((c (name-char "Backspace")))
    (or (not c)
	(let ((s (concatenate 'string "|" (string c) "|")))
	  (eval `(signals-error (read-from-string ',s) reader-error)))))
  t)

(def-syntax-test syntax.multiple-escape.invalid.rubout
  (let ((c (name-char "Rubout")))
    (or (not c)
	(let ((s (concatenate 'string "|" (string c) "|")))
	  (eval `(signals-error (read-from-string ',s) reader-error)))))
  t)
