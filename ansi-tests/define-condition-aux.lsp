;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Mar  9 05:40:13 2003
;;;; Contains: Auxiliary functions for testing DEFINE-CONDITION

(in-package :cl-test)

(defun make-def-cond-name (name &rest suffixes)
  (intern (apply #'concatenate 'string (string name) "/"
		 (mapcar #'string suffixes))
	  :cl-test))

(defmacro define-condition-with-tests (name-symbol
				       parents slot-specs &rest options)

  "Create a condition and some associated tests."

  (assert (symbolp name-symbol))
  (dolist (parent parents) (assert (symbolp parent)))
  
  (let ((name (symbol-name name-symbol)))
    `(progn
       (define-condition ,name-symbol ,parents ,slot-specs ,@options)
       ,@(loop for parent in (adjoin 'condition parents)
	       collect
	       `(deftest ,(make-def-cond-name name "IS-SUBTYPE-OF/" parent)
		  (subtypep* ',name-symbol ',parent)
		  t t))
       ,@(loop for parent in (adjoin 'condition parents)
	       collect
	       `(deftest ,(make-def-cond-name name "IS-SUBTYPE-OF-2/" parent)
		  (check-all-subtypep ',name-symbol ',parent)
		  nil))
       ,@(loop for parent in (adjoin 'condition parents)
	       collect
	       `(deftest ,(make-def-cond-name name
					      "IS-NOT-SUPERTYPE-OF/" parent)
		  (subtypep* ',parent ',name-symbol)
		  nil t))
       ,@(loop for parent in (adjoin 'condition parents)
	       collect
	       `(deftest ,(make-def-cond-name name "IS-A/" parent)
		  (let ((c (make-condition ',name-symbol)))
		    (notnot-mv (typep c ',parent)))
		  t))
       ,@(loop for parent in (adjoin 'condition parents)
	       collect
	       `(deftest ,(make-def-cond-name name "IS-SUBCLASS-OF/" parent)
		  (subtypep* (find-class ',name-symbol)
			     (find-class ',parent))
		  t t))
       ,@(loop for parent in (adjoin 'condition parents)
	       collect
	       `(deftest ,(make-def-cond-name name
					      "IS-NOT-SUPERCLASS-OF/" parent)
		  (subtypep* (find-class ',parent)
			     (find-class ',name-symbol))
		  nil t))
       ,@(loop for parent in (adjoin 'condition parents)
	       collect
	       `(deftest ,(make-def-cond-name name "IS-A-MEMBER-OF-CLASS/"
					      parent)
		  (let ((c (make-condition ',name-symbol)))
		    (notnot-mv (typep c (find-class ',parent))))
		  t))
       )))

