;;; Load test files

;;; Tests of symbols
(load "load-symbols.lsp")

;;; Tests of evaluation and compilation
(load "load-eval-and-compile.lsp")

;;; Tests of data and control flow
(load "load-data-and-control-flow.lsp")

;;; Tests of iteration forms
(load "load-iteration.lsp")

;;; Tests of conditions
(load "load-conditions.lsp")

;;; Tests of conses
(load "load-cons.lsp")

;;; Tests on arrays
(load "load-arrays.lsp")

;;; Tests of hash tables

(load "hash-table.lsp")
(load "make-hash-table.lsp")
  ; More to come

;;; Tests of packages

#-ecl (load "packages.lsp")

;;; Tests of sequences
(load "load-sequences.lsp")

;;; Tests of structures
(load "load-structures.lsp")

;;; Tests of types and classes
(load "load-types-and-class.lsp")

;;; Tests of the reader
(load "reader-test.lsp")

;;; Tests of strings
(load "load-strings.lsp")

;;; Tests for character functions
(compile-and-load "char-aux.lsp")
(load "character.lsp")
(load "char-compare.lsp")

;;; Tests of system construction
(load "features.lsp")
