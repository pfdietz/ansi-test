;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Feb 23 04:40:33 2004
;;;; Contains: File to load tests of the lisp printer

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(load "printer/copy-pprint-dispatch.lsp")

(load "printer/print-integers.lsp")
(load "printer/print-ratios.lsp")
(load "printer/print-floats.lsp")
(load "printer/print-complex.lsp")
(load "printer/print-characters.lsp")
(load "printer/print-symbols.lsp")
(load "printer/print-strings.lsp")
(load "printer/print-cons.lsp")
(load "printer/print-backquote.lsp")
(load "printer/print-bit-vector.lsp")
(load "printer/print-vector.lsp")
(load "printer/print-array.lsp")
(load "printer/print-random-state.lsp")
(load "printer/print-pathname.lsp")
(load "printer/print-structure.lsp")
(load "printer/printer-control-vars.lsp")
(load "printer/pprint-dispatch.lsp")
(load "printer/pprint-fill.lsp")
(load "printer/pprint-linear.lsp")
(load "printer/pprint-tabular.lsp")
(load "printer/pprint-indent.lsp")
(load "printer/pprint-logical-block.lsp")
(load "printer/pprint-exit-if-list-exhausted.lsp")
(load "printer/pprint-newline.lsp")
(load "printer/pprint-tab.lsp")
(load "printer/print-unreadable-object.lsp")
(load "printer/write.lsp")
(load "printer/print.lsp")
(load "printer/pprint.lsp")
(load "printer/prin1.lsp")
(load "printer/princ.lsp")
(load "printer/write-to-string.lsp")
(load "printer/prin1-to-string.lsp")
(load "printer/princ-to-string.lsp")
(load "printer/print-level.lsp")
(load "printer/print-length.lsp")

(load "printer/load-format.lsp")
