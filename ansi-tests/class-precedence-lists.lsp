;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Jun  4 20:18:29 2003
;;;; Contains: Tests that builtin classes have the right CPLs

(in-package :cl-test)

(unless (fboundp 'class-precedence-list)
  (defgeneric class-precedence-list (x)
    (:method-combination list)
    .
    #.(loop for s in *cl-types-that-are-classes-symbols*
	    collect
	    `(:method list ((x ,s)) ',s))))

(defmacro def-cpl-test (objform expected-cpl)
  `(deftest ,(intern (concatenate 'string
				  (symbol-name (first expected-cpl))
				  "-CPL")
		     :cl-test)
     (let* ((obj ,objform)
	    (cpl (class-precedence-list obj)))
       (and (eql (first cpl) ',(first expected-cpl))
	    (is-noncontiguous-sublist-of cpl ',expected-cpl)))
     t))

;;; Condition types

(defmacro def-cond-cpl-test (expected-cpl)
  `(def-cpl-test (make-condition ',(first expected-cpl)) ,expected-cpl))

(def-cond-cpl-test (arithmetic-error error serious-condition condition t))
(def-cond-cpl-test (cell-error error serious-condition condition t))
(def-cond-cpl-test (condition t))
(def-cond-cpl-test (control-error error serious-condition condition t))
(def-cond-cpl-test (division-by-zero arithmetic-error error
				     serious-condition condition t))
(def-cond-cpl-test (error serious-condition condition t))
(def-cond-cpl-test (file-error error serious-condition condition t))
(def-cond-cpl-test (floating-point-inexact arithmetic-error error
					   serious-condition condition t))
(def-cond-cpl-test (floating-point-invalid-operation
		    arithmetic-error error serious-condition condition t))
(def-cond-cpl-test (floating-point-overflow arithmetic-error error
					    serious-condition condition t))
(def-cond-cpl-test (floating-point-underflow arithmetic-error error
					     serious-condition condition t))
(def-cond-cpl-test (package-error error serious-condition condition t))
(def-cond-cpl-test (parse-error error serious-condition condition t))
(def-cond-cpl-test (print-not-readable error serious-condition condition t))
(def-cond-cpl-test (program-error error serious-condition condition t))
(def-cond-cpl-test (reader-error parse-error stream-error
				 error serious-condition condition t))
(def-cond-cpl-test (serious-condition condition t))
(def-cond-cpl-test (simple-condition condition t))
(def-cond-cpl-test (simple-error simple-condition error serious-condition
				 condition t))
(def-cond-cpl-test (simple-type-error simple-condition type-error
				      error serious-condition condition t))
(def-cond-cpl-test (simple-warning simple-condition warning condition t))
(def-cond-cpl-test (storage-condition serious-condition condition t))
(def-cond-cpl-test (stream-error error serious-condition condition t))
(def-cond-cpl-test (style-warning warning condition t))
(def-cond-cpl-test (type-error error serious-condition condition t))
(def-cond-cpl-test (unbound-slot cell-error error serious-condition condition t))
(def-cond-cpl-test (unbound-variable cell-error error serious-condition condition t))
(def-cond-cpl-test (undefined-function cell-error error serious-condition condition t))
(def-cond-cpl-test (warning condition t))

(def-cpl-test (make-array '(2 3 4)) (array t))
(def-cpl-test (make-array '(10) :element-type 'bit :adjustable t :fill-pointer 5)
  (bit-vector vector array sequence t))
(def-cpl-test (make-broadcast-stream) (broadcast-stream stream t))
(when (typep (class-of 'symbol) 'built-in-class)
  (def-cpl-test (class-of 'symbol) (built-in-class class standard-object t)))





