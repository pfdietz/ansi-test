;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Apr  8 20:03:45 1998
;;;; Contains: Tests on readtables (just started, very incomplete)

(in-package :cl-test)

(deftest read-symbol.1
    (let ((*package* (find-package "CL-TEST")))
      (ignore-errors (read-from-string "a")))
  a 1)

(deftest read-symbol.2
    (let ((*package* (find-package "CL-TEST")))
      (ignore-errors (read-from-string "|a|")))
  |a| 3)

(deftest read-symbol.3
  (multiple-value-bind (s n)
      (let () (ignore-errors (read-from-string "#:abc")))
    (not
     (and (symbolp s)
	  (eql n 5)
	  (not (symbol-package s))
	  (string-equal (symbol-name s) "abc"))))
  nil)

(deftest read-symbol.4
  (multiple-value-bind (s n)
      (let () (ignore-errors (read-from-string "#:|abc|")))
    (not
     (and (symbolp s)
	  (eql n 7)
	  (not (symbol-package s))
	  (string= (symbol-name s) "abc"))))
  nil)

(deftest read-symbol.5
  (multiple-value-bind (s n)
      (let () (ignore-errors (read-from-string "#:||")))
    (if (not (symbolp s))
	s
      (not (not
	    (and (eql n 4)
		 (not (symbol-package s))
		 (string= (symbol-name s) ""))))))
  t)

(deftest read-symbol.6
  (let ((str "cl-test::abcd0123"))
    (multiple-value-bind (s n)
	(ignore-errors (read-from-string str))
      (if (not (symbolp s))
	  s
	(not (not
	      (and (eql n (length str))
		   (eqt (symbol-package s) (find-package :cl-test))
		   (string-equal (symbol-name s)
				 "abcd0123")))))))
  t)

(deftest read-symbol.7
  (multiple-value-bind (s n)
      (let () (ignore-errors (read-from-string ":ABCD")))
    (if (not (symbolp s))
	s
      (not (not
	    (and (eql n 5)
		 (eqt (symbol-package s) (find-package "KEYWORD"))
		 (string-equal (symbol-name s)
			       "ABCD"))))))
  t)
	     
(defun read-symbol.9-body (natoms maxlen)
  (let* ((chars (concatenate 'string
		  "abcdefghijklmnopqrstuvwxyz"
		  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
		  "0123456789"
		  "<,>.?/\"':;[{]}~`!@#$%^&*()_-+= \\|"))
	 (nchars (length chars)))
    (loop
	for i from 1 to natoms
	count
	  (let* ((len (random (1+ maxlen)))
		 (actual-len 0)
		 (s (make-string (+ 2 (* 2 len))))
		 (s2 (make-string len)))
	    (loop for j from 0 to (1- len) do
		  (let ((c (elt chars (random (max 1 (1- nchars))))))
		    (when (member c '(#\| #\\))
		      (setf (elt s actual-len) #\\)
		      (incf actual-len))
		    (setf (elt s actual-len) c)
		    (setf (elt s2 j) c)
		    (incf actual-len)))
	    (let ((actual-string (subseq s 0 actual-len)))
	      (multiple-value-bind (sym nread)
		  (ignore-errors (read-from-string
			(concatenate 'string
			  "#:|" actual-string "|")))
		(unless (and (symbolp sym)
			     (eql nread (+ 4 actual-len))
			     (string-equal s2 (symbol-name sym)))
		  (let ((*print-readably* t))
		    (format t "Symbol read failed: ~S (~S) read as ~S~%"
			    actual-string s2 sym))
		  t)))))))

(deftest read-symbol.9
    (read-symbol.9-body 1000 100)
  0)

(deftest read-symbol.10
  (handler-case
   (not (not
	 (equal (symbol-name
		 (read-from-string
		  (with-output-to-string (s)
					 (write (make-symbol ":")
						:readably t
						:stream s))))
		":")))
   (error (c) c))
  t)

(deftest read-float.1
  (eqlt -0.0 (- 0.0))
  t)
