;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Nov 28 06:43:51 2002
;;;; Contains: Aux. functions for cl-symbols.lsp

(in-package :cl-test)

(declaim (optimize (safety 3)))

(defun is-external-symbol-of (sym package)
  (multiple-value-bind (sym2 status)
      (find-symbol (symbol-name sym) package)
    (and (eqt sym sym2)
	 (eqt status :external))))

(defun test-if-not-in-cl-package (str)
  (multiple-value-bind (sym status)
      (find-symbol (string-upcase str) 'common-lisp)
      (declare (ignore sym))
      (or
       ;; Symbol not present in the common lisp package
       (not status)
       ;; Check if it has any properties whose indicators are
       ;; external in any of the standard packages or are accessible
       ;; in CL-USER
       (and (eqt status :external)
	    (let ((plist (symbol-plist sym)))
	      (loop for e = plist then (cddr e)
		    while e
		    for indicator = (car e)
		    when (and (symbolp indicator)
			      (or (is-external-symbol-of indicator
							 "COMMON-LISP")
				  (is-external-symbol-of indicator "KEYWORD")
				  (eqt indicator (find-symbol
						  (symbol-name indicator)
						  "COMMON-LISP-USER"))))
		    collect indicator))))))

(defun safe-symbol-name (sym)
  (catch-type-error (symbol-name sym)))

(defun safe-make-symbol (name)
  (catch-type-error (make-symbol name)))