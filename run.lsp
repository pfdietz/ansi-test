;-*- Mode:     Lisp -*-

(load "gclload1.lsp")
(pushnew :mutation *features*)
(compile-and-load "random/random-int-form.lsp")
(compile-and-load "random/random-type-prop.lsp")
#+sbcl (compile-and-load "random/mutate.lsp")
(load "random/random-type-prop-tests.lsp")
(funcall (symbol-function (intern "INIT-RANDOM-STATE" "CL-TEST")))

