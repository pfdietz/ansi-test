;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Sep  2 07:24:42 2004
;;;; Contains: Tests for NAMESTRING

(in-package :cl-test)

(deftest namestring.1
  (let* ((vals (multiple-value-list (namestring "namestring.lsp")))
	 (s (first vals)))
    (if (and (null (cdr vals))
	     (stringp s)
	     (equal (namestring s) s))
	:good
      vals))
  :good)

(deftest namestring.2
  (do-special-strings
   (s "namestring.lsp" nil)
   (let ((ns (namestring s)))
     (assert (stringp ns))
     (assert (string= (namestring ns) ns))))
  nil)

