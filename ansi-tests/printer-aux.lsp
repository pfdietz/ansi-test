;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Feb 23 06:20:00 2004
;;;; Contains: Auxiliary functions and macros for printer tests

(in-package :cl-test)

(defmacro def-print-test (name form result &rest bindings)
  `(deftest ,name
     (equalt
      (with-standard-io-syntax
       (let ,bindings
	 (with-output-to-string (*standard-output*) (prin1 ,form))))
      ,result)
     t))

;;; Function to test readable of printed forms, under random settings
;;; of various printer control variables.
;;;
;;; Return NIL if obj printed and read properly, or a list containing
;;; the object and the printer variable bindings otherwise.  They key
;;; argument TEST is used to compared the reread object and obj.

(defun randomly-check-readability (obj &key (test #'equal))
  ;; Generate random printer-control values
  (with-standard-io-syntax
   (let ((*print-array* (coin))
	 (*print-base* (+ 2 (random 34)))
	 (*print-radix* (coin))
	 (*print-case* (random-from-seq #(:upcase :downcase :capitalize)))
	 (*print-circle* (coin))
	 (*print-escape* (coin))
	 (*print-gensym* (coin))
	 (*print-level* (random 50))
	 (*print-length* (random 50))
	 (*print-lines* (random 50))
	 (*print-miser-width* (and (coin) (random 100)))
	 (*print-pretty* (coin))
	 (*print-right-margin* (and (coin) (random 100)))
	 (*print-readably* t)
	 )
     (let* ((str (with-output-to-string (s) (write obj :stream s)))
	    (obj2 (let ((*read-base* *print-base*))
		    (handler-case
		     (read-from-string str)
		     (reader-error () :error)))))
       (unless (funcall test obj obj2)
	 (list
	  (list obj obj2
		(list '*print-array* *print-array*)
		(list '*print-base* *print-base*)
		(list '*print-radix* *print-radix*)
		(list '*print-case* *print-case*)
		(list '*print-circle* *print-circle*)
		(list '*print-escape* *print-escape*)
		(list '*print-gensym* *print-gensym*)
		(list '*print-level* *print-level*)
		(list '*print-length* *print-length*)
		(list '*print-lines* *print-lines*)
		(list '*print-miser-width* *print-miser-width*)
		(list '*print-pretty* *print-pretty*)
		(list '*print-right-margin* *print-right-margin*))))))))

       
