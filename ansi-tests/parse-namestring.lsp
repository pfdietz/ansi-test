;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Aug 14 13:59:18 2004
;;;; Contains: Tests of PARSE-NAMESTRING

(in-package :cl-test)

;;; "Parsing a null string always succeeds, producing a pathname
;;;  with all components (except the host) equal to nil."

(deftest parse-namestring.1
  (let ((vals (multiple-value-list (parse-namestring ""))))
    (assert (= (length vals) 2))
    (let ((pn (first vals))
	  (pos (second vals)))
      (values
       (pathname-directory pn)
       (pathname-device pn)
       (pathname-name pn)
       (pathname-type pn)
       (pathname-version pn)
       pos)))
  nil nil nil nil nil 0)

;;; Error tests

(deftest parse-namestring.error.1
  (signals-error (parse-namestring) program-error)
  t)

(deftest parse-name-string.error.2
  (signals-error (parse-namestring "" nil *default-pathname-defaults* :foo nil) program-error)
  t)

(deftest parse-name-string.error.3
  (signals-error (parse-namestring "" nil *default-pathname-defaults* :start) program-error)
  t)


