;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May  9 11:21:25 1998
;;;; Contains: Common code for creating structure tests

(in-package :cl-test)
(declaim (optimize (safety 3)))

(defun make-struct-test-name (structure-name n)
  (declare (type (or string symbol character) structure-name)
	   (type fixnum n))
  (setf structure-name (string structure-name))
  (intern (concatenate 'string
	    structure-name
	    "-STRUCT-TEST-"
	    (princ-to-string n))
	  :cl-test))

(defun make-struct-p-fn (structure-name)
  (setf structure-name (string structure-name))
  (intern (concatenate 'string
	    structure-name
	    "-P")
	  :cl-test))

(defun make-struct-copy-fn (structure-name)
   (setf structure-name (string structure-name))
   (intern (concatenate 'string
	     "COPY-"
	     structure-name)
	   :cl-test))

(defun make-struct-field-fn (structure-name field-name)
  "Make field accessor for a field in a structure"
  (setf structure-name (string structure-name))
  (setf field-name (string field-name))
  (intern (concatenate 'string
	    structure-name "-" field-name)
	  :cl-test))

(defun make-struct-make-fn (structure-name)
  "Make the make- function for a structure"
  (setf structure-name (string structure-name))
  (intern (concatenate 'string
	    "MAKE-" structure-name)
	  :cl-test))  

(defun create-instance-of-type (type)
  "Return an instance of a type.  Signal an error if
  it can't figure out a value for the type."
  (cond
   ((eqt type t)  ;; anything
    'a)
   ((eqt type 'symbol)
    'b)
   ((eqt type 'null) nil)
   ((eqt type 'boolean) t)
   ((eqt type 'keyword) :foo)
   ((eqt type nil) (error "Cannot obtain element of type ~S~%" type))
   ((eqt type 'cons) (cons 'a 'b))
   ((eqt type 'list) (list 1 2 3))
   ((eqt type 'fixnum) 17)
   ((eqt type 'bignum)
    (let ((x 1))
      (loop until (typep x 'bignum)
	  do (setq x (* 2 x)))
      x))
   ((and (symbolp type)
	 (typep type 'structure-class))
    (let ((make-fn
	   (intern (concatenate 'string "MAKE-" (symbol-name type))
		   (symbol-package type))))
      (eval (list make-fn))))
   ((eqt type 'character) #\w)
   ((eqt type 'base-char) #\z)
   ((member type '(integer unsigned-byte signed-byte)) 35)
   ((eqt type 'bit) 1)
   ((and (consp type)
	 (consp (cdr type))
	 (consp (cddr type))
	 (null (cdddr type))
	 (eqt (car type) 'integer)
	 (integerp (second type)))
    (second type))
   ((member type '(float single-float long-float double-float short-float))
    0.0)
   ((and (consp type)
	 (eqt (car type) 'member)
	 (consp (cdr type)))
    (second type))
   ((and (consp type)
	 (eqt (car type) 'or)
	 (consp (second type)))
    (create-instance-of-type (second type)))
   (t (error "Cannot generate element for type ~S~%" type))))

;;
;; There are a number of standardized tests for
;; structures.  The following macro generates the
;; structure definition and the tests.
;;

(defmacro defstruct-with-tests
    (name-and-options &body slot-descriptions-and-documentation)
"Construct standardized tests for a defstruct, and also
do the defstruct."
  (let* ((doc-string
	 (when (and (consp slot-descriptions-and-documentation)
		    (stringp (car slot-descriptions-and-documentation)))
	   (car slot-descriptions-and-documentation)))
	 (slot-descriptions
	  (if doc-string (cdr slot-descriptions-and-documentation)
	    slot-descriptions-and-documentation))
	 (name (if (consp name-and-options)
		   (car name-and-options)
		 name-and-options))
	 (options (if (consp name-and-options)
		      (cdr name-and-options)
		    nil))
	 (slot-names
	  (loop
	      for x in slot-descriptions collect
		(if (consp x) (car x) x)))
	 (make-fn (make-struct-make-fn name))
	 (p-fn (make-struct-p-fn name))
	 (copy-fn (make-struct-copy-fn name))
	 ;; a list of initial values
	 (initial-value-alist
	  (loop
	      for slot-desc in slot-descriptions
	      collect
		(let ((slot-name (if (consp slot-desc)
				     (car slot-desc)
				   slot-desc))
		      (slot-attrs (if (consp slot-desc)
				      (cdr slot-desc)
				    nil)))
		  (when (and (consp slot-attrs)
			     (not (keywordp slot-attrs)))
		    (pop slot-attrs))
		  (let ((type (getf slot-attrs :type)))
		    (if type
			(cons slot-name (create-instance-of-type type))
		      (cons slot-name (gensym)))))))
	 )
      `(eval-when (compile load eval)
	 (defstruct ,name-and-options
	    ,@slot-descriptions-and-documentation)
	 
	 ;; Test that structure is of the correct type
	 (deftest ,(make-struct-test-name name 1)
	     (not (not (typep (,make-fn) (quote ,name))))
	   t)
	 
	 ;; Test that the -P predicate exists
	 (deftest ,(make-struct-test-name name 2)
	     (not (not (,p-fn (,make-fn))))
	   t)
	 
	 ;; Test that the elements of *universe* are not
	 ;; of this type
	 (deftest ,(make-struct-test-name name 3)
	     (count-if (function ,p-fn) *universe*)
	   0)
	 
	 (deftest ,(make-struct-test-name name 4)
	     (count-if (function (lambda (x) (typep x (quote ,name))))
		       *universe*)
	   0)
	 
	 ;; Check that the fields can be read after being initialized
	 (deftest ,(make-struct-test-name name 5)
	     ,(let ((inits nil)
		    (tests nil)
		    (var (gensym "X")))
		(loop
		    for (slot-name . initval) in initial-value-alist
		    do
		      (setf inits
			(list* (intern (string slot-name) "KEYWORD")
			       (list 'quote initval)
			       inits))
		      (push `(eql (quote ,initval)
				  (,(make-struct-field-fn name slot-name)
				   ,var))
			    tests))
		`(let ((,var (,(make-struct-make-fn name) . ,inits)))
		   (and ,@tests t)))
	   t)
	 
	 ;; Check that two invocations return different structures
	 (deftest ,(make-struct-test-name name 6)
	     ,(let ((make-fn (make-struct-make-fn name)))
		`(eqt (,make-fn) (,make-fn)))
	   nil)
	 
	 ;; Check that we can setf the fields
	 (deftest ,(make-struct-test-name name 7)
	     ,(let* ((var (gensym "X"))
		     (var2 (gensym "T"))
		     (tests
		      (loop
			  for (slot-name . initval) in initial-value-alist
			  collect
			    (let ((field-fn (make-struct-field-fn name slot-name)))
			      `(let ((,var2 (quote ,initval)))
				 (setf (,field-fn ,var) ,var2)
				 (eql (,field-fn ,var) ,var2))))))
		`(let ((,var (,(make-struct-make-fn name))))
		   (and ,@tests t)))
	   t)
	 
	 ;; Check that the copy function properly copies fields
	 (deftest ,(make-struct-test-name name 8)
	     ,(let* ((var (gensym "X"))
		     (var2 (gensym "Y")))
		`(let ((,var (,(make-struct-make-fn name)
			      ,@(loop
				    for (slot-name . initval) in initial-value-alist
				    nconc (list (intern (string slot-name) "KEYWORD")
						`(quote ,initval))))))
		   (let ((,var2 (,(make-struct-copy-fn name) ,var)))
		     (and
		      (not (eql ,var ,var2))
		      ,@(loop
			    for (slot-name . initval) in initial-value-alist
			    collect
			      (let ((fn (make-struct-field-fn name slot-name)))
				`(eql (,fn ,var) (,fn ,var2))))
		      t))))
	   t)
	 )))
