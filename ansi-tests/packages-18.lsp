;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 25 08:07:31 1998
;;;; Contains: Package test code, part 18

(in-package :cl-test)
(declaim (optimize (safety 3)))

(declaim (special *universe*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; packagep, typep * 'package

(deftest packagep-1
  (loop
   for x in *universe* count
   (unless (eq (not (not (packagep x)))
	       (not (not (typep x 'package))))
	   (format t
		   "(packagep ~S) = ~S, (typep x 'package) = ~S~%"
		   x (packagep x) x (typep x 'package))
	   t))
  0)

;; *package* is always a package

(deftest packagep-2
  (not (not (packagep *package*)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package-error

(deftest package-error-1
    (not (not
	  (typep (make-condition 'package-error :package "CL")
	   'package-error)))
  t)

(deftest package-error-2
    (not (not
	  (typep (make-condition 'package-error
		   :package (find-package "CL"))
	   'package-error)))
  t)

(deftest package-error-3
    (subtypep 'package-error 'error)
  t t)

(deftest package-error-4
   (not (not
	  (typep (make-condition 'package-error
		   :package (find-package '#:|CL|))
	   'package-error)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package-error-package

(deftest package-error-package-1
    (handler-case
	(eq (find-package (package-error-package
			   (make-condition 'package-error
			     :package "CL")))
	    (find-package "CL"))
      (error (c) c))
  t)

(deftest package-error-package-2
    (handler-case 
	(eq (find-package (package-error-package
			   (make-condition 'package-error
			     :package (find-package "CL"))))
	    (find-package "CL"))
      (error (c) c))
  t)

(deftest package-error-package-3
    (handler-case
	(eq (find-package (package-error-package
			   (make-condition 'package-error
			     :package '#:|CL|)))
	    (find-package "CL"))
      (error (c) c))
  t)

(deftest package-error-package-4
    (handler-case
	(eq (find-package (package-error-package
			   (make-condition 'package-error
			     :package #\A)))
	    (find-package "A"))
      (error (c) c))
  t)

