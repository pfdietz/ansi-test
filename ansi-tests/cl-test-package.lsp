;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 14 10:13:21 1998
;;;; Contains: CL test case package definition

(defpackage :cl-test
  (:use :cl :rt)
  #+gcl (:use defpackage)
  (:nicknames)
  (:import-from "COMMON-LISP-USER" "COMPILE-AND-LOAD")
  (:export))

#+cmu (import 'cl::quit :cl-test)


