;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue May 25 07:15:02 2004
;;;; Contains: Tests of printing random states

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(deftest print.random-state.1
  (let* ((rs1 *random-state*)
	 (rs2 (with-standard-io-syntax
	       (read-from-string
		(write-to-string rs1 :readably t)))))
    (values
     (notnot (random-state-p rs2))
     (is-similar rs1 rs2)))
  t t)
