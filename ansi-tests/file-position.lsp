;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Jan 22 03:02:31 2004
;;;; Contains: Tests of FILE-POSITION

(in-package :cl-test)

(deftest file-position.1
  (with-open-file (is "file-position.lsp":direction :input)
		  (file-position is))
  0)

(deftest file-position.2
  (with-open-file (is "file-position.lsp":direction :input)
		  (values
		   (multiple-value-list
		    (notnot-mv (file-position is :start)))
		   (file-position is)))
			      
  (t) 0)

(deftest file-position.3
  (with-open-file (is "file-position.lsp":direction :input)
		  (values
		   (multiple-value-list
		    (notnot-mv (file-position is :end)))
		   (notnot (> (file-position is) 0))))
  (t) t)

(deftest file-position.4
  (with-open-file
   (is "file-position.lsp":direction :input)
   (values
    (file-position is)
    (read-char is)
    (notnot (> (file-position is) 0))))
  0 #\; t)

