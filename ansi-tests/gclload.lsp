;;; Uncomment the next line to make MAKE-STRING and MAKE-SEQUENCE
;;; tests require that a missing :initial-element argument defaults
;;; to a single value, rather than leaving the string/sequence filled
;;; with arbitrary legal garbage.
;; (pushnew :ansi-tests-strict-initial-element *features*)

#+:(and clisp win32)            ; w2k exits on (disassemble 'car)
(without-package-lock ("SYS")
 (defun sys::disassemble-machine-code (a b c)
   (format t "~&<~S ~S ~S>~%" a b c)))

(load "gclload1.lsp")
(load "gclload2.lsp")
(in-package :cl-test)
(time (regression-test:do-tests))
