;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Nov 29 05:06:57 2003
;;;; Contains: Tests of the function PATHNAME

(in-package :cl-test)

(deftest pathname.1
  (loop for x in *pathnames*
	always (eq x (pathname x)))
  t)

(deftest pathname.2
  (equalt #p"ansi-aux.lsp" (pathname "ansi-aux.lsp"))
  t)

(deftest pathname.3
  (let ((s (open "ansi-aux.lsp" :direction :input)))
    (prog1 (equalt (truename (pathname s)) (truename #p"ansi-aux.lsp"))
      (close s)))
  t)

(deftest pathname.4
  (let ((s (open "ansi-aux.lsp" :direction :input)))
    (close s)
    (equalt (truename (pathname s)) (truename #p"ansi-aux.lsp")))
  t)

(deftest pathname.5
  (loop for x in *logical-pathnames*
	always (eq x (pathname x)))
  t)

;;; Error tests

(deftest pathname.error.1
  (classify-error (pathname))
  program-error)

(deftest pathname.error.2
  (classify-error (pathname (first *pathnames*) nil))
  program-error)