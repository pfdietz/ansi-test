#+:ecl (si::package-lock (find-package "COMMON-LISP") nil)

(load "compile-and-load.lsp")
(load "rt-package.lsp")
(compile-and-load "rt.lsp")
;;; (unless (probe-file "rt.o") (compile-file "rt.lsp"))
;;; (load "rt.o")
(load "cl-test-package.lsp")
(in-package :cl-test)
(compile-and-load "ansi-aux-macros.lsp")
(load "universe.lsp")
(compile-and-load "random-aux.lsp")
(compile-and-load "ansi-aux.lsp")
;;; (unless (probe-file "ansi-aux.o") (compile-file "ansi-aux.lsp"))
;;; (load "ansi-aux.o")

(load "cl-symbol-names.lsp")
(load "notes.lsp")

#+cmu (setq *compile-verbose* nil *compile-print* nil
	    ext:*gc-verbose* nil)
#+gcl (setq compiler:*suppress-compiler-notes* t
	    compiler:*suppress-compiler-warnings* t
	    *compile-verbose* nil
	    *compile-print* nil
	    compiler:*compile-verbose* nil
	    compiler:*compile-print* nil
	    *load-verbose* nil)

