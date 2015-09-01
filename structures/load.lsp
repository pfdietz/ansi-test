;;; Tests of structures

(let ((*default-pathname-defaults* (pathname *load-pathname*)))
  (load "structure-00.lsp")
  (load "structures-01.lsp")
  (load "structures-02.lsp")
  (load "structures-03.lsp")
  (load "structures-04.lsp")
)
