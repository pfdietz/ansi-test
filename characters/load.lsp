;;;; Character tests
(compile-and-load "ANSI-TESTS:AUX;char-aux.lsp")

(let ((*default-pathname-defaults* (pathname *load-pathname*)))
  (load "character.lsp")
  (load "char-compare.lsp")
  (load "name-char.lsp")
)
