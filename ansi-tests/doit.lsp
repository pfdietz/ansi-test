;;; Uncomment the next line to make MAKE-STRING and MAKE-SEQUENCE
;;; tests require that a missing :initial-element argument defaults
;;; to a single value, rather than leaving the string/sequence filled
;;; with arbitrary legal garbage.
;; (pushnew :ansi-tests-strict-initial-element *features*)

#+(and clisp (or win32 cygwin))       ; w2k exits on (disassemble 'car)
(without-package-lock ("SYS")
 (defun sys::disassemble-machine-code (a b c)
   (format t "~&<~S ~S ~S>~%" a b c)))

#+allegro (run-shell-command "rm -f *.fasl")
#+cmucl (run-program "rm -f *.x86f")

(load "gclload1.lsp")
(load "gclload2.lsp")

#+allegro
(progn
  (rt:disable-note :nil-vectors-are-strings)
  (rt:disable-note :standardized-package-nicknames)
  (rt:disable-note :type-of/strict-builtins)
  (rt:disable-note :assume-no-simple-streams)
  (rt:disable-note :assume-no-gray-streams))

(in-package :cl-test)
(time (regression-test:do-tests))

#+allegro (cl-user::exit)
#+(or cmucl sbcl gcl armedbear) (cl-user::quit)
