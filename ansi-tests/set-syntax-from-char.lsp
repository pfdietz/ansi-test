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

(loop for c across "!\"#$%&'()*,;<=>?@[]^_`~{}+-/abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
      do (eval `(def-set-syntax-from-char-alphabetic-trait-test ,c)))

(defmacro def-set-syntax-from-char-invalid-trait-test (c)
  `(def-set-syntax-from-char-trait-test ,c
     (let ((form (subst c 'c '(signals-error
			       (let* ((*package* (find-package "CL-TEST"))
				      (sym (read-from-string (concatenate 'string (string c) "Z"))))
				 sym)
			       reader-error))))
       (multiple-value-list (eval form)))
     (t)))

(loop for name in '("Backspace" "Tab" "Newline" "Linefeed" "Page" "Return" "Space" "Rubout")
      do (eval `(def-set-syntax-from-char-invalid-trait-test ,name)))

		    
	
