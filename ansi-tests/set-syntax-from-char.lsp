;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jan 29 06:37:18 2005
;;;; Contains: Tests of SET-SYNTAX-FROM-CHAR

(in-package :cl-test)

(compile-and-load "reader-aux.lsp")

(defmacro def-set-syntax-from-char-test (name form &body expected-values)
  `(deftest ,name
     (with-standard-io-syntax
      (let ((*readtable* (copy-readtable nil)))
	(setf (readtable-case *readtable*) :preserve)
	,form))
     ,@expected-values))

;;; Test that constituent traits are not altered when a constituent character
;;; syntax type is set

(defmacro def-set-syntax-from-char-trait-test (c test-form expected-value)
  (setq c (typecase c
	    (character c)
	    ((or string symbol) (name-char (string c)))
	    (t nil)))
  (when c
    (format t "~A ~A~%" c (char-name c))
    `(def-set-syntax-from-char-test
       ,(intern (concatenate 'string "SET-SYNTAX-FROM-CHAR-TRAIT-X-" (or (char-name c)
									 (string c)))
		:cl-test)
       (let ((c ,c))
	 (values
	  (set-syntax-from-char c #\X)
	  ,test-form))
       t ,expected-value)))

(defmacro def-set-syntax-from-char-alphabetic-trait-test (c)
  `(def-set-syntax-from-char-trait-test ,c
     (let* ((*package* (find-package "CL-TEST"))
	    (sym (read-from-string (string c))))
       (list (let ((sym2 (find-symbol (string c))))
	       (or (eqt sym sym2)
		   (list sym sym2)))
	     (or (equalt (symbol-name sym) (string c))
		 (list (symbol-name sym) (string c)))))
     (t t)))

(loop for c across "\\|!\"#$%&'()*,;<=>?@[]^_`~{}+-/abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
      do (eval `(def-set-syntax-from-char-alphabetic-trait-test ,c)))

;;; The invalid constituent character trait of invalid and whitespace characters
;;; is exposed when they are turned into constituent characters

(defmacro def-set-syntax-from-char-invalid-trait-test (c)
  `(def-set-syntax-from-char-trait-test ,c
     (handler-case
      (let* ((*package* (find-package "CL-TEST"))
	     (sym (read-from-string (concatenate 'string (string c) "Z"))))
	sym)
      (reader-error (c) (declare (ignore c)) :good))
     :good))

(loop for name in '("Backspace" "Tab" "Newline" "Linefeed" "Page" "Return" "Space" "Rubout")
      do (eval `(def-set-syntax-from-char-invalid-trait-test ,name)))

;;; Turning characters into single escape characters

(deftest set-syntax-from-char.single-escape
  (loop for c across +standard-chars+
	nconc
	(with-standard-io-syntax
	 (let ((*readtable* (copy-readtable nil))
	       (*package* (find-package "CL-TEST")))
	   (let ((results
		  (list
		   (set-syntax-from-char c #\\)
		   (read-from-string (concatenate 'string (list c #\Z))))))
	     (unless (equal results '(t |Z|))
	       (list c results))))))
  nil)

(deftest set-syntax-from-char.multiple-escape
  (loop for c across +standard-chars+
	nconc
	(with-standard-io-syntax
	 (let ((*readtable* (copy-readtable nil))
	       (*package* (find-package "CL-TEST")))
	   (let ((results
		  (list
		   (set-syntax-from-char c #\|)
		   (handler-case
		    (read-from-string (concatenate 'string (list c #\Z c)))
		    (error (c) c))
		   (handler-case
		    (read-from-string (concatenate 'string (list c #\z #\|)))
		    (error (c) c))
		   (handler-case
		    (read-from-string (concatenate 'string (list #\| #\Z c)))
		    (error (c) c)))))
	     (unless (or (eql c #\Z) (eql c #\z) (equal results '(t |Z| |z| |Z|)))
	       (list c results))))))
  nil)

(deftest set-syntax-from-char.semicolon
  (loop for c across +standard-chars+
	nconc
	(with-standard-io-syntax
	 (let ((*readtable* (copy-readtable nil))
	       (*package* (find-package "CL-TEST"))
	       (expected (if (eql c #\0) '1 '0))
	       (c2 (if (eql c #\0) #\1 #\0)))
	   (let ((results
		  (list
		   (set-syntax-from-char c #\;)
		   (handler-case
		    (read-from-string (concatenate 'string (list c2 c #\2)))
		    (error (c) c))
		   (handler-case
		    (read-from-string (concatenate 'string (list c2 c #\2 #\Newline #\3)))
		    (error (c) c))
		   (handler-case
		    (read-from-string (concatenate 'string (list c #\2 #\Newline c2)))
		    (error (c) c)))))
	     (unless (equal results (list t expected expected expected))
	       (list c results))))))
  nil)
		   