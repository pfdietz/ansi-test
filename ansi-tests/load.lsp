;; Get the MK package
;; I've hardwired a path here; fix for your system
;; I assume the package is already compiled.
(unless (find-package "MK")
  (load #.(concatenate 'string "../defsys30/defsystem."
		     #+cmu (C::BACKEND-FASL-FILE-TYPE C::*TARGET-BACKEND*)
		     #+allegro "fasl"
		     #+(or akcl gcl) "o")))

(load "rt/rt.system")
(mk::load-system "rt")
(mk::compile-system "cltest")
(in-package :cl-test)


