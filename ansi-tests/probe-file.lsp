;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jan  5 20:46:29 2004
;;;; Contains: Tests of PROBE-FILE

(in-package :cl-test)

(deftest probe-file.1
  (probe-file #p"nonexistent")
  nil)

(deftest probe-file.2
  (let ((s (open #p"probe-file.lsp" :direction :input)))
    (prog1
	(equalp (truename #p"probe-file.lsp")
		(probe-file s))
      (close s)))
  t)

(deftest probe-file.3
  (let ((s (open #p"probe-file.lsp" :direction :input)))
    (close s)
    (equalp (truename #p"probe-file.lsp")
	    (probe-file s)))
  t)

(deftest probe-file.error.1
  (classify-error (probe-file))
  program-error)

(deftest probe-file.error.2
  (classify-error (probe-file #p"probe-file.lsp" nil))
  program-error)

(deftest probe-file.error.3
  (classify-error (probe-file (make-pathname :name :wild)))
  file-error)



  
