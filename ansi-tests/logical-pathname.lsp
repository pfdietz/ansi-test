;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Dec 30 19:05:01 2003
;;;; Contains: Tests of LOGICAL-PATHNAME

(in-package :cl-test)

(deftest logical-pathname.1
  (loop for x in *logical-pathnames*
	always (eql x (logical-pathname x)))
  t)


;;; Error tests

(deftest logical-pathname.error.1
  (loop for x in *mini-universe*
	unless (or (stringp x)
		   (typep x 'stream)
		   (typep x 'logical-pathname)
		   (eval `(signals-error (logical-pathname ',x)
					 type-error)))
	collect x)
  nil)

(deftest logical-pathname.error.2
  ;; Doesn't specify a host
  (signals-error (logical-pathname "FOO.TXT") type-error)
  t)

(deftest logical-pathname.error.3
  (signals-error
   (with-open-file (s #p"logical-pathname.lsp" :direction :input)
		   (logical-pathname s))
   type-error)
  t)

