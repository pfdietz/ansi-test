;-*- Mode:     Lisp -*-
;;;; Copyright 1998 Motorola, Inc.  All right reserved.
;;;; Motorola Internal Use Only.
;;;;
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar  7 10:32:13 1998
;;;; Contains: Package definition for RT system (new form)

(eval-when
 ;;(:execute :compile-toplevel :load-toplevel)
 (load eval compile)
 (defpackage :rt
    (:nicknames)
    (:use :cl)
    (:export
     "DEFTEST" "GET-TEST" "DO-TEST" "REM-TEST"
     "REM-ALL-TESTS" "DO-TESTS" "PENDING-TESTS"
     "CONTINUE-TESTING" "*TEST*" "*DO-TESTS-WHEN-DEFINED*")))

(in-package :rt)
