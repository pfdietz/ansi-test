;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Dec 17 21:10:53 2002
;;;; Contains: Package definition for RT

(eval-when
 ;;(:execute :compile-toplevel :load-toplevel)
 (load eval compile)
 (defpackage :rt
    (:use :cl)
    (:export
     "*DO-TESTS-WHEN-DEFINED*"
     "*TEST*"
     "CONTINUE-TESTING"
     "DEFTEST"
     "DO-TEST"
     "DO-TESTS"
     "GET-TEST"
     "PENDING-TESTS"
     "REM-ALL-TESTS"
     "REM-TEST"
     )))

(in-package :rt)
