;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr  6 17:00:30 2003
;;;; Contains: Tests for EVAL-WHEN

;;; The following test was suggested by Sam Steingold,
;;; so I've created this file to hold it.

(in-package :cl-test)

(defvar *eval-when.1-collector*)

(deftest eval-when.1
  
  (let ((forms nil) all (ff "generated-eval-when-test-file.lisp"))
    (dolist (c '(nil (:compile-toplevel)))
      (dolist (l '(nil (:load-toplevel)))
	(dolist (x '(nil (:execute)))
	  (push `(eval-when (,@c ,@l ,@x)
		   (push '(,@c ,@l ,@x) *eval-when.1-collector*))
		forms))))
    (dolist (c '(nil (:compile-toplevel)))
      (dolist (l '(nil (:load-toplevel)))
	(dolist (x '(nil (:execute)))
	  (push `(let () (eval-when (,@c ,@l ,@x)
			   (push '(let ,@c ,@l ,@x) *eval-when.1-collector*)))
		forms))))
    (with-open-file (o ff :direction :output)
		    (dolist (f forms)
		      (prin1 f o)
		      (terpri o)))
    (let ((*eval-when.1-collector* nil))
      (load ff)
      (push (cons "load source" *eval-when.1-collector*) all))
    (let ((*eval-when.1-collector* nil))
      (compile-file ff)
      (push (cons "compile source" *eval-when.1-collector*) all))
    (let ((*eval-when.1-collector* nil))
      (load (compile-file-pathname ff))
      (push (cons "load compiled" *eval-when.1-collector*) all))
    (delete-file ff)
    (delete-file (compile-file-pathname ff))
    #+clisp (delete-file (make-pathname :type "lib" :defaults ff))
    (nreverse all))
  
  (("load source"
    (:execute) (:load-toplevel :execute) (:compile-toplevel :execute)
    (:compile-toplevel :load-toplevel :execute)
    (let :execute) (let :load-toplevel :execute)
    (let :compile-toplevel :execute)
    (let :compile-toplevel :load-toplevel :execute))
   ("compile source"
    (:compile-toplevel) (:compile-toplevel :execute)
    (:compile-toplevel :load-toplevel)
    (:compile-toplevel :load-toplevel :execute))
   ("load compiled"
    (:load-toplevel) (:load-toplevel :execute)
    (:compile-toplevel :load-toplevel)
    (:compile-toplevel :load-toplevel :execute)
    (let :execute) (let :load-toplevel :execute)
    (let :compile-toplevel :execute)
    (let :compile-toplevel :load-toplevel :execute))))

;;; More EVAL-WHEN tests to go here

