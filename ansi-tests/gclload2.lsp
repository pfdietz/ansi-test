;;; Load individual test files

;;; Tests of symbols
(compile-and-load "cl-symbols-aux.lsp")
(load "cl-symbol-names.lsp")
(load "cl-symbols.lsp")

;;; Tests of evaluation and compilation
(load "eval.lsp")
(load "eval-and-compile.lsp")
(load "compile.lsp")
(load "lambda.lsp")

;;; Tests of data and control flow

(load "data-and-control-flow.lsp")
(load "places.lsp")

(load "and.lsp")
(load "block.lsp")
(load "call-arguments-limit.lsp")
(load "case.lsp")
(load "catch.lsp")
(load "ccase.lsp")
(load "constantly.lsp")
(load "complement.lsp")
(load "cond.lsp")
(load "ctypecase.lsp")
(load "defconstant.lsp")
(load "define-modify-macro.lsp")
(load "defparameter.lsp")
(load "defvar.lsp")
(load "destructuring-bind.lsp")
(load "ecase.lsp")
(load "equal.lsp")
(load "equalp.lsp")
(load "eql.lsp")
(load "etypecase.lsp")
(load "every.lsp")
(load "fboundp.lsp")
(load "flet.lsp")
(load "fmakunbound.lsp")
(load "funcall.lsp")
(load "function.lsp")
(load "functionp.lsp")
(load "identity.lsp")
(load "if.lsp")
(load "labels.lsp")
(load "lambda-list-keywords.lsp")
(load "lambda-parameters-limit.lsp")
(load "let.lsp")
(load "macrolet.lsp")
(load "multiple-value-bind.lsp")
(load "multiple-value-call.lsp") ;; include multiple-value-list
(load "multiple-value-prog1.lsp")
(load "multiple-value-setq.lsp")
(load "nil.lsp")
(load "not-and-null.lsp")
(load "notany.lsp")
(load "notevery.lsp")
(load "nth-value.lsp")
(load "or.lsp")
(load "prog.lsp")
(load "prog1.lsp")
(load "prog2.lsp")
(load "progn.lsp")
(load "progv.lsp")
(load "some.lsp")
(load "t.lsp")
(load "tagbody.lsp")
(load "typecase.lsp")
(load "unless.lsp")
(load "unwind-protect.lsp")
(load "values.lsp")
(load "when.lsp")

;;; Tests of iteration forms
(load "iteration.lsp")
(load "loop.lsp")
(load "loop1.lsp")
(load "loop2.lsp")
(load "loop3.lsp")
(load "loop4.lsp")
(load "loop5.lsp")
(load "loop6.lsp")
(load "loop7.lsp")
(load "loop8.lsp")
(load "loop9.lsp")
(load "loop10.lsp")
(load "loop11.lsp")
(load "loop12.lsp")
(load "loop13.lsp")
(load "loop14.lsp")
(load "loop15.lsp")
(load "loop16.lsp")
(load "loop17.lsp")

;;; Tests of conses

(load "cons-test-01.lsp")
(load "cons-test-02.lsp")
(load "cons-test-03.lsp")
(load "cons-test-04.lsp")
(load "cons-test-05.lsp")
(load "cons-test-06.lsp")
(load "cons-test-07.lsp")
(load "cons-test-08.lsp")
(load "cons-test-09.lsp")
(load "cons-test-10.lsp")
(load "cons-test-11.lsp")
(load "cons-test-12.lsp")
(load "cons-test-13.lsp")
(load "cons-test-14.lsp")
(load "cons-test-15.lsp")
(load "cons-test-16.lsp")
(load "cons-test-17.lsp")
(load "cons-test-18.lsp")
(load "cons-test-19.lsp")
(load "cons-test-20.lsp")
(load "cons-test-21.lsp")
(load "cons-test-22.lsp")
(load "cons-test-23.lsp")
(load "cons-test-24.lsp")
(load "cons-test-25.lsp")

;;; Tests on arrays
(load "make-array.lsp")

;;; Tests of packages

(load "packages.lsp")

;;; Tests of sequences

(load "copy-seq.lsp")
(load "elt.lsp")
(load "fill.lsp")
(load "fill-strings.lsp")
(load "make-sequence.lsp")
(load "map.lsp")
(load "map-into.lsp")
(load "reduce.lsp")
(load "count.lsp")
(load "count-if.lsp")
(load "count-if-not.lsp")
(load "reverse.lsp")
(load "nreverse.lsp")
(load "sort.lsp")
(load "find.lsp")
(load "find-if.lsp")
(load "find-if-not.lsp")
(load "position.lsp")
(compile-and-load "search-aux.lsp")
(load "search-list.lsp")
(load "search-vector.lsp")
(load "search-bitvector.lsp")
(load "search-string.lsp")
(load "mismatch.lsp")
(load "replace.lsp")
(compile-and-load "subseq-aux.lsp")
(load "subseq.lsp")
(load "substitute.lsp")
(load "substitute-if.lsp")
(load "substitute-if-not.lsp")
(load "nsubstitute.lsp")
(load "nsubstitute-if.lsp")
(load "nsubstitute-if-not.lsp")
(load "concatenate.lsp")
(load "merge.lsp")
(compile-and-load "remove-aux.lsp")
(load "remove.lsp")  ;; also related funs
(compile-and-load "remove-duplicates-aux.lsp")
(load "remove-duplicates.lsp")  ;; also delete-duplicates

;;; Tests of structures

(load "structure-00.lsp")
(load "structures-01.lsp")
(load "structures-02.lsp")

;;; Tests of types and classes

(load "types-and-class.lsp")
(load "coerce.lsp")

;;; Tests of the reader

(load "reader-test.lsp")

;;; Tests of strings

(load "char-schar.lsp")
(load "string.lsp")
(load "string-upcase.lsp")
(load "string-downcase.lsp")
(load "string-capitalize.lsp")
(load "nstring-upcase.lsp")
(load "nstring-downcase.lsp")
(load "nstring-capitalize.lsp")
(load "string-trim.lsp")
(load "string-left-trim.lsp")
(load "string-right-trim.lsp")

;;; Tests of string comparison functions
(compile-and-load "string-aux.lsp")
(load "string-comparisons.lsp")
(load "make-string.lsp")

;;; Tests for character functions
(compile-and-load "char-aux.lsp")
(load "character.lsp")
(load "char-compare.lsp")

;;; Tests of system construction
(load "features.lsp")
