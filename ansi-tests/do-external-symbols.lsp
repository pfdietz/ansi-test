;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Feb 21 18:26:08 2004
;;;; Contains: Tests of DO-EXTERNAL-SYMBOLS

(in-package :cl-test)

(compile-and-load "package-aux.lsp")

(declaim (optimize (safety 3)))

(defun collect-external-symbols (pkg)
  (remove-duplicates
   (sort-symbols
    (let ((all nil))
      (do-external-symbols (x pkg all) (push x all))))))

(deftest do-external-symbols.1
    (collect-external-symbols "DS1")
  (DS1:A DS1:B))

(deftest do-external-symbols.2
    (collect-external-symbols "DS2")
  (DS2:A DS2:G DS2:H))

(deftest do-external-symbols.3
    (collect-external-symbols "DS3")
  (DS1:A DS3:B DS2:G DS3:I DS3:J DS3:K))

(deftest do-external-symbols.4
    (collect-external-symbols "DS4")
  ())

(deftest do-external-symbols.5
    (equalt (collect-external-symbols "KEYWORD")
	    (collect-symbols "KEYWORD"))
  t)

;; Test that do-external-symbols works without
;; a return value (and that the default return value is nil)

(deftest do-external-symbols.6
  (do-external-symbols (s "DS1") (declare (ignore s)) t)
  nil)

;; Test that do-external-symbols works without
;; a package being specified

(deftest do-external-symbols.7
  (let ((x nil)
	(*package* (find-package "DS1")))
    (declare (special *package*))
    (list
     (do-external-symbols (s) (push s x))
     (sort-symbols x)))
  (nil (DS1:A DS1:B)))

;; Test that the tags work in the tagbody,
;;  and that multiple statements work

(deftest do-external-symbols.8
  (handler-case
   (let ((x nil))
     (list
      (do-external-symbols
       (s "DS1")
       (when (equalt (symbol-name s) "A") (go bar))
       (push s x)
       (go foo)
       bar
       (push t x)
       foo)
      (sort-symbols x)))
   (error (c) c))
  (NIL (DS1:B T)))

(def-macro-test do-external-symbols.error.1
  (do-external-symbols (x "CL")))
