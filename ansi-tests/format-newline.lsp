;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Nov 27 08:07:16 2004
;;;; Contains: Tests of ~<newline>

(in-package :cl-test)
(compile-and-load "printer-aux.lsp")

(deftest format.newline.1
  (let ((nl (string #\Newline)))
    (format nil (concatenate 'string "~" nl "   X")))
  "X")

(deftest format.newline.2
  (let ((nl (string #\Newline)))
    (format nil (concatenate 'string "A~:" nl " X")))
  "A X")

(deftest format.newline.3
  (let ((nl (string #\Newline)))
    (format nil (concatenate 'string "A~@" nl " X")))
  #.(concatenate 'string "A" (string #\Newline) "X"))


