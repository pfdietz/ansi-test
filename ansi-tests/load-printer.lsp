;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Feb 23 04:40:33 2004
;;;; Contains: File to load tests of the lisp printer

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(load "copy-pprint-dispatch.lsp")

(load "print-integers.lsp")
(load "print-ratios.lsp")
(load "print-floats.lsp")
(load "print-complex.lsp")
(load "print-characters.lsp")
(load "print-symbols.lsp")
(load "print-strings.lsp")
