;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Dec 17 21:10:53 2002
;;;; Contains: Package definition for RT

(eval-when
 #-gcl (:execute :compile-toplevel :load-toplevel)
 #+gcl (load eval compile)
 (defpackage :regression-test
    (:use :cl)
    (:nicknames :rtest #-lispworks :rt)
    (:export
     #:*do-tests-when-defined*
     #:*compile-tests*
     #:*test*
     #:continue-testing
     #:deftest
     #:do-test
     #:do-tests
     #:get-test
     #:pending-tests
     #:rem-all-tests
     #:rem-test
     #:defnote
     )))

(in-package :regression-test)
