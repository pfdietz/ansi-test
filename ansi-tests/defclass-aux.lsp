;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Mar 24 03:40:24 2003
;;;; Contains: Auxiliary functions for testing CLOS

(in-package :cl-test)

(defun make-defclass-test-name (&rest args)
  (intern (apply #'concatenate 'string (mapcar #'string args))
	  (find-package :cl-test)))

(defparameter *defclass-slot-readers* nil)
(defparameter *defclass-slot-writers* nil)
(defparameter *defclass-slot-accessors* nil)

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
	 (slot-names
	  (loop for slot-spec in slot-specifiers
		collect (cond
			 ((symbolp slot-spec) slot-spec)
			 (t (assert (consp slot-spec))
			    (assert (symbolp (car slot-spec)))
			    (car slot-spec)))))
	 (slot-options
	  (loop for slot-spec in slot-specifiers
		collect (if (consp slot-spec)
			    (cdr slot-spec)
			  nil)))
	 (readers 
	  (loop for slot-option in slot-options
		append (collect-properties slot-option :reader)))
	 (writers
	  (loop for slot-option in slot-options
		append (collect-properties slot-option :writer)))
	 (accessors
	  (loop for slot-option in slot-options
		append (collect-properties slot-option :accessor)))
	 (allocations
	  (loop for slot-option in slot-options
		collect (or (get :allocation slot-option)
			    :instance)))
	 (initargs
	  (loop for slot-option in slot-options
		collect (collect-properties slot-option :initarg)))
	 (types
	  (loop for slot-option in slot-options
		collect (collect-properties slot-option :type)))
	 (initforms
	  (loop for slot-option in slot-options
		collect (collect-properties slot-option :initform)))
	 )

    (declare (ignorable readers writers accessors allocations
			initargs types initforms default-initargs
			doc))

    (assert (loop for e in types always (< (length e) 2)))
    (assert (loop for e in initforms always (< (length e) 2)))

    (setf *defclass-slot-readers* (append readers *defclass-slot-readers*))
    (setf *defclass-slot-writers* (append writers *defclass-slot-writers*))
    (setf *defclass-slot-accessors*
	  (append accessors *defclass-slot-accessors*))

    `(progn

       (eval-when (load eval compile)
	 (ignore-errors (defclass ,@(cdr args))))

       (deftest ,(make-defclass-test-name class-name
					  "-IS-IN-ITS-METACLASS")
	 (notnot-mv (typep (find-class ',class-name) ',metaclass))
	 t)

       ,@(when (eq metaclass 'standard-class)
	   `((deftest ,(make-defclass-test-name class-name
						"S-ARE-STANDARD-OBJECTS")
	       (subtypep* ',class-name 'standard-object)
	       t t)))

       ,@(loop for slot-name in slot-names
	       collect
	       `(deftest ,(make-defclass-test-name class-name
						   "-HAS-SLOT-NAMED-"
						   slot-name)
		  (notnot-mv (slot-exists-p (make-instance ',class-name)
					    ',slot-name))
		  t))

       (deftest ,(make-defclass-test-name class-name
					  "-INSTANCES-CLASS-IS-THIS-ONE")
	 (let ((class (find-class ',class-name)))
	   (eqlt (class-of (allocate-instance class)) class))
	 t)

       )))

(defmacro generate-slot-tests ()
  "Generate generic tests from the read/writer/accessor functions
   for slots from defclass-with-tests."
  (let ((funs (remove-duplicates
	       (append *defclass-slot-readers*
		       *defclass-slot-writers*
		       *defclass-slot-accessors*))))
    `(progn
       (deftest class-readers/writers/accessors-are-generic-functions
	 (loop for sym in ',funs
	       unless (typep (symbol-function sym) 'generic-function)
	       collect sym)
	 nil)
       
       (deftest class-accessors-have-generic-setf-functions
	 (append
	  ,@(loop for sym in *defclass-slot-accessors*
		  collect
		  `(and (not (typep (function (setf ,sym))
				    'generic-function))
			'(,sym))))
	 nil))))
