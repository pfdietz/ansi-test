;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 14 10:13:21 1998
;;;; Contains: CL test case package definition

#|
(progn
  (defpackage :cl-test
    (:use :cl :regression-test)
    (:nicknames)
    (:shadow #:handler-case #:handler-bind)
    (:import-from "COMMON-LISP-USER" #:compile-and-load)
    (:export #:random-from-seq #:random-case #:coin #:random-permute))
  (let ((s (find-symbol "QUIT" "CL-USER")))
    (when s (import s :cl-test))))
|#

(let* ((name :cl-test)
       (pkg (find-package name)))
  (unless pkg (setq pkg (make-package name :use '(#-wcl :cl #+wcl :lisp :regression-test))))
  (let ((*package* pkg))
    (shadow '(#:handler-case #:handler-bind))
    (import '(#-wcl common-lisp-user::compile-and-load
	      #+wcl user::compile-and-load)
	    pkg)
    (export (mapcar #'intern (mapcar #'symbol-name '(#:random-from-seq #:random-case #:coin #:random-permute)))))
  (let ((s (find-symbol "QUIT" "CL-USER")))
    (when s (import s :cl-test))))


