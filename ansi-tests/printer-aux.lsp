;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Feb 23 06:20:00 2004
;;;; Contains: Auxiliary functions and macros for printer tests

(in-package :cl-test)

(defmacro def-print-test (name form result &rest bindings)
  `(deftest ,name
     (equalpt
      (my-with-standard-io-syntax
	(let ((*print-readably* nil))
	  (let ,bindings
	    (with-output-to-string (*standard-output*) (prin1 ,form)))))
      ,result)
     t))

;;; Function to test readable of printed forms, under random settings
;;; of various printer control variables.
;;;
;;; Return NIL if obj printed and read properly, or a list containing
;;; the object and the printer variable bindings otherwise.  They key
;;; argument TEST is used to compared the reread object and obj.

(defvar *random-read-check-debug* nil
  "When set to true, RANDOMLY-CHECK-READABILITY will dump out parameter
   settings before trying a test.  This is intended for cases where the
   error that occurs is fatal.")

(defun randomly-check-readability (obj &key
				       (can-fail nil)
				       (test #'equal)
				       (readable t)
				       (circle nil circle-p)
				       (escape nil escape-p)
				       (gensym nil gensym-p)
				       (debug *random-read-check-debug*))
  (declare (type function test))
  ;; Generate random printer-control values
  (my-with-standard-io-syntax
   (let ((*print-array* (coin))
	 (*print-base* (+ 2 (random 34)))
	 (*print-radix* (coin))
	 (*print-case* (random-from-seq #(:upcase :downcase :capitalize)))
	 (*print-circle* (if circle-p circle (coin)))
	 (*print-escape* (if escape-p escape (coin)))
	 (*print-gensym* (if gensym-p gensym (coin)))
	 (*print-level* (random 50))
	 (*print-length* (if readable (random 50) nil))
	 (*print-lines* (if readable (random 50) nil))
	 (*print-miser-width* (and (coin) (random 100)))
	 (*print-pretty* (coin))
	 (*print-right-margin* (and (coin) (random 100)))
	 (*print-readably* readable)
	 (*read-default-float-format* (rcase (1 'short-float) (1 'single-float)
					     (1 'double-float) (1 'long-float)
					     (1 *read-default-float-format*)))
	 (*readtable* (copy-readtable))
	 (readcase (random-from-seq #(:upcase :downcase :preserve :invert)))
	 )
     (flet ((%params ()
		     (list (list '*print-readably* *print-readably*)
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
			   (list '*print-right-margin* *print-right-margin*)
			   (list '*read-default-float-format* *read-default-float-format*)
			   (list 'readtable-case readcase))))
       (when debug
	 (let ((params (%params)))
	   (with-standard-io-syntax (format *debug-io* "~%~A~%" params)))
	 (finish-output *debug-io*))
       
       (setf (readtable-case *readtable*) readcase)
       (let* ((str (handler-case
		    (with-output-to-string (s) (write obj :stream s))
		    (print-not-readable
		     ()
		     (if can-fail
			 (return-from randomly-check-readability nil)
		       ":print-not-readable-error"))))
	      (obj2 (let ((*read-base* *print-base*))
		      (handler-case
		       (let ((*readtable* (if *print-readably*
					      (copy-readtable nil)
					    *readtable*)))
			 (read-from-string str))
		       (reader-error () :error)))))
	 (unless (funcall test obj obj2)
	   (list
	    (list* obj str obj2 (%params)
		   ))))))))

(defun parse-escaped-string (string)
  "Parse a string into a list of either characters (representing
   themselves unescaped) or lists (<char> :escape) (representing
   escaped characters.)"
  (assert (stringp string) () "Not a string: ~A" string)
  (let ((result nil)
	(len (length string))
	(index 0))
    (prog
     ()
     normal ; parsing in normal mode
     (when (= index len) (return))
     (let ((c (elt string index)))
       (cond ((eql c #\\)
	      (assert (< (incf index) len)
		      ()
		      "End of string after \\")
	      (push `(,(elt string index) :escaped) result)
	      (incf index)
	      (go normal))
	     ((eql c #\|)
	      (incf index)
	      (go multiple-escaped))
	     (t (push c result)
		(incf index)
		(go normal))))

     multiple-escaped   ; parsing inside |s
     (assert (< index len) () "End of string inside |")
     (let ((c (elt string index)))
       (cond ((eq c #\|)
	      (incf index)
	      (go normal))
	     (t
	      (push `(,c :escaped) result)
	      (incf index)
	      (go multiple-escaped)))))
    (nreverse result)))

(defun escaped-equal (list1 list2)
  "Determine that everything escaped in list1 is also escaped
   in list2, and that the characters are also the same."
  (and (= (length list1) (length list2))
       (loop for e1 in list1
	     for e2 in list2
	     for is-escaped1 = (and (consp e1) (eq (cadr e1) :escaped))
	     for is-escaped2 = (and (consp e2) (eq (cadr e2) :escaped))
	     for c1 = (if is-escaped1 (car e1) e1)
	     for c2 = (if is-escaped2 (car e2) e2)
	     always
	     (and (if is-escaped1 is-escaped2 t)
		  (char= c1 c2)))))

(defun similar-uninterned-symbols (s1 s2)
  (and (symbolp s1)
       (symbolp s2)
       (null (symbol-package s1))
       (null (symbol-package s2))
       (string= (symbol-name s1)
		(symbol-name s2))))

(defun make-random-cons-tree (size)
  (if (<= size 1)
      (rcase
       (5 nil)
       (1 (random 1000))
       (1 (random 1000.0))
       (2 (random-from-seq #(a b c d e f g |1| |2| |.|))))
    (let ((s1 (1+ (random (1- size)))))
      (cons (make-random-cons-tree s1)
	    (make-random-cons-tree (- size s1))))))

(defun make-random-vector (size)
  (if (> size 1)
      (let* ((nelems (min (1- size) (1+ (random (max 2 (floor size 4))))))
	     (sizes (mapcar #'1+ (random-partition* (- size nelems 1) nelems))))
	(make-array nelems :initial-contents (mapcar #'make-random-vector sizes)))
    (rcase
     (1 (random-from-seq #(a b c d e f g)))
     (1 (- (random 2001) 1000))
     (1 (random 1000.0))
     )))
