;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Mar 24 03:40:24 2003
;;;; Contains: Auxiliary functions for testing CLOS

(in-package :cl-test)

(defun make-defclass-test-name (&rest args)
  (intern (apply #'concatenate 'string (mapcar #'string args))
	  (find-package :cl-test)))

(defmacro defclass-with-tests
  (&whole args
	  class-name superclasses slot-specifiers
	  &rest class-options)
  
  (assert (typep class-name '(and (not null) symbol)))
  (assert (listp superclasses))
  (assert (every #'(lambda (x) (typep x '(and (not null) symbol)))
		 superclasses))
  (assert (listp slot-specifiers))
  (assert (every #'(lambda (s)
		     (or (symbolp s) (and (consp s) (symbolp (car s)))))
		 slot-specifiers))
  (assert (every #'(lambda (x)
		     (and (consp x)
			  (member (car x) '(:default-initargs
					    :documentation
					    :metaclass))))
		 class-options))
  (assert (eql (length class-options)
	       (length (remove-duplicates class-options))))
  
  (let* ((default-initargs (second (assoc :default-initargs class-options)))
	 (metaclass (or (second (assoc :metaclass class-options))
			'standard-class))
	 (doc (second (assoc :documentation class-options)))
	 )
	 
    `(progn
       
       (eval-when (load eval compile)
	 (ignore-errors (defclass ,@args)))

       (deftest ,(make-defclass-test-name class-name
					  "-IS-IN-ITS-METACLASS")
	 (notnot-mv (typep (find-class ',class-name) ',metaclass))
	 t)

       ,@(when (eq metaclass 'standard-class)
	   `((deftest ,(make-defclass-test-name class-name
						"S-ARE-STANDARD-OBJECTS")
	       (subtypep* ,class-name 'standard-object)
	       t t)))

       ;;; More tests here

       )))
       
	  

	       
     
     