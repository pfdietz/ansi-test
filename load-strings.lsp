;;; Tests of strings

(load "strings/char-schar.lsp")
(load "strings/string.lsp")
(load "strings/base-string.lsp")
(load "strings/simple-string.lsp")
(load "strings/simple-base-string.lsp")
(load "strings/simple-string-p.lsp")
(load "strings/stringp.lsp")
(load "strings/string-upcase.lsp")
(load "strings/string-downcase.lsp")
(load "strings/string-capitalize.lsp")
(load "strings/nstring-upcase.lsp")
(load "strings/nstring-downcase.lsp")
(load "strings/nstring-capitalize.lsp")
(load "strings/string-trim.lsp")
(load "strings/string-left-trim.lsp")
(load "strings/string-right-trim.lsp")

;;; Tests of string comparison functions
(compile-and-load "string-aux.lsp")
(load "strings/string-comparisons.lsp")
(load "strings/make-string.lsp")
