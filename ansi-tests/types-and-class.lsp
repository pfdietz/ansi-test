;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Mar 19 21:48:39 1998
;;;; Contains: Data for testing type and class inclusions

;; We should check for every type that NIL is a subtype, and T a supertype

(in-package :cl-test)
(use-package :rt)

(declaim (optimize (safety 3)))

(deftest boolean-type-1
  (handler-case
    (not (not (typep nil 'boolean)))
    (error (c) c))
  t)

(deftest boolean-type-2
  (handler-case
    (not (not (typep t 'boolean)))
    (error (c) c))
  t)

(defun is-t-or-nil (e)
  (or (eq e t) (eq e nil)))

(deftest boolean-type-3
    (check-type-predicate 'is-t-or-nil 'boolean)
  0)

;; Two type inclusions on booleans
;; have been conditionalized to prevent
;; some tests from doing too badly on CMU CL on x86
;; These should get removed when I get a more up to date
;; image for that platform -- pfd

(defvar *subtype-table*
'(
(symbol t)
#-(and cmu (not sparc)) (boolean symbol)
(standard-object t)
(function t)
(compiled-function function)
(generic-function function)
(standard-generic-function generic-function)
(class standard-object)
(built-in-class class)
(structure-class class)
(standard-class class)
(method standard-object)
(standard-method method)
(structure-object t)
(method-combination t)
(condition t)
(serious-condition condition)
(error serious-condition)
(type-error error)
(simple-type-error type-error)
(simple-condition condition)
(simple-type-error simple-condition)
(parse-error error)
(hash-table t)
(cell-error error)
(unbound-slot cell-error)
(warning condition)
(style-warning warning)
(storage-condition serious-condition)
(simple-warning warning)
(simple-warning simple-condition)
(keyword symbol)
(unbound-variable cell-error)
(control-error error)
(program-error error)
(undefined-function cell-error)
(package t)
(package-error error)
(random-state t)
(number t)
(real number)
(complex number)
(float real)
(short-float float)
(single-float float)
(double-float float)
(long-float float)
(rational real)
(integer rational)
(ratio rational)
(signed-byte integer)
(integer signed-byte)
(unsigned-byte signed-byte)
(bit unsigned-byte)
(fixnum integer)
(bignum integer)
(bit fixnum)
(arithmetic-error error)
(division-by-zero arithmetic-error)
(floating-point-invalid-operation arithmetic-error)
(floating-point-inexact arithmetic-error)
(floating-point-overflow arithmetic-error)
(floating-point-underflow arithmetic-error)
(character t)
(base-char character)
(standard-char base-char)
(extended-char character)
(sequence t)
(list sequence)
(null list)
#-(and cmu (not sparc)) (null boolean)
(cons list)
(array t)
(simple-array array)
(vector sequence)
(vector array)
(string vector)
(bit-vector vector)
(simple-vector vector)
(simple-vector simple-array)
(simple-bit-vector bit-vector)
(simple-bit-vector simple-array)
(base-string string)
(simple-string string)
(simple-string simple-array)
(simple-base-string base-string)
(simple-base-string simple-string)
(pathname t)
(logical-pathname pathname)
(file-error error)
(stream t)
(broadcast-stream stream)
(concatenated-stream stream)
(echo-stream stream)
(file-stream stream)
(string-stream stream)
(synonym-stream stream)
(two-way-stream stream)
(stream-error error)
(end-of-file stream-error)
(print-not-readable error)
(readtable t)
(reader-error parse-error)
(reader-error stream-error)
))

(deftest types-3
    (count-if
     #'(lambda (pair)
	 (let ((t1 (first pair))
	       (t2 (second pair)))
	   (cond
	    ((not (subtypep t1 t2))
	     (format t "~%~S not a subtype of ~S" t1 t2)
	     t)
	    (t nil))))
     *subtype-table*)
  0)

(defconstant +float-types+ '(long-float double-float short-float single-float))

(defun types-4-body ()
  (let ((parent-table (make-hash-table :test #'equal))
	(types nil))
    (loop
	for p in *subtype-table* do
	  (let ((tp (first p))
		(parent (second p)))
	    (pushnew tp types)
	    (pushnew parent types)
	    (let ((parents (gethash tp parent-table)))
	      (pushnew parent parents)
	      ;; (format t "~S ==> ~S~%" tp parent)
	      (loop
		  for pp in (gethash parent parent-table) do
		    ;; (format t "~S ==> ~S~%" tp pp)
		    (pushnew pp parents))
	      (setf (gethash tp parent-table) parents))))
    ;; parent-table now contains lists of ancestors
    (loop
	for tp in types sum
	  (let ((parents (gethash tp parent-table)))
	    (loop
		for tp2 in types sum
		  (cond
		   ((and (not (eq tp tp2))
			 (not (eq tp2 'standard-object))
			 (not (eq tp2 'structure-object))
			 (not (member tp2 parents))
			 (subtypep tp tp2)
			 (not (and (member tp +float-types+)
				   (member tp2 +float-types+)))
			 (not (and (eq tp2 'structure-object)
				   (member 'standard-object parents))))
		    (format t "~%Improper subtype: ~S of ~S"
			    tp tp2)
		    1)
		   (t 0)))))
    ))

(deftest types-4
    (types-4-body)
  0)

(deftest types-5
    (subtypep* 'simple-base-string 'sequence)
  t t)

(defun types-6-body ()
  (loop
      for p in *subtype-table* count
	(let ((tp (car p)))
	  (cond
	   ((and (not (member tp '(sequence cons list t)))
		 (not (subtypep tp 'atom)))
	    (format t "~%Not an atomic type: ~S" tp)
	    t)))))

(deftest types-6
    (types-6-body)
  0)

(defvar *disjoint-types-list*
    '(cons symbol array
      number character hash-table function readtable package
      pathname stream random-state condition restart))

(deftest types-7
    (loop
	for tp in *disjoint-types-list* sum
	  (loop for tp2 in *disjoint-types-list* count
		(and (not (eq tp tp2))
		     (subtypep tp tp2))))
  0)

(deftest types-8
    (loop
	for tp in *disjoint-types-list* count
	  (cond
	   ((and (not (eq tp 'cons))
		 (not (subtypep tp 'atom)))
	    (format t "~%Should be atomic, but isn't: ~S" tp)
	    t)))
  0)

(defvar *type-list* nil)
(defvar *supertype-table* nil)

(defun types-9-body ()
  (let ((tp-list (append '(keyword atom list)
			 (loop for p in *subtype-table* collect (car p))))
	(result-list))
    (setf tp-list (remove-duplicates tp-list))
    (setf *type-list* tp-list)
    (let ((subs (make-hash-table :test #'eq))
	  (sups (make-hash-table :test #'eq)))
      (loop
	  for x in tp-list do
	    (loop
		for y in tp-list do
		  (multiple-value-bind (result good)
		      (subtypep x y)
		    (declare (ignore good))
		    (when result
		      (pushnew x (gethash y subs))
		      (pushnew y (gethash x sups))))))
      (setf *supertype-table* sups)
      (loop
	  for x in tp-list do
	    (let ((sub-list (gethash x subs))
		  (sup-list (gethash x sups)))
	      (loop
		  for t1 in sub-list do
		    (loop
			for t2 in sup-list do
			  (multiple-value-bind (result good)
			      (subtypep t1 t2)
			    (when (and good (not result))
			      (pushnew (list t1 x t2) result-list
				       :test #'equal)))))))
      
      result-list)))

(deftest types-9
    (types-9-body)
  nil)

(defun types-9a-body ()
  (cond
     ((not (and *type-list* *supertype-table*))
      (format nil "Run test type-9 first~%")
      nil)
     (t
      (loop
	  for tp in *type-list*
	  sum
	    (let ((sups (gethash tp *supertype-table*)))
	      (loop
		  for x in *universe*
		  sum
		    (handler-case
		    (cond
		     ((not (typep x tp)) 0)
		     (t
		      (loop
			  for tp2 in sups
			  count
			    (handler-case
				(and (not (typep x tp2))
				     (progn
				       (format t "Found element of ~S not in ~S ~S~%"
					       tp tp2 x)
				       t))
			      (condition (c) (format t "Error ~S occured: ~S~%"
						c tp2)
				t)))))
		    (condition (c) (format t "Error ~S occured: ~S~%" c tp)
		      1))))))))

(deftest types-9a
    (types-9a-body)
  0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; deftype

(defun even-size-p (a)
  (some #'evenp (array-dimensions a)))

(deftype even-array (&optional type size)
  `(and (array ,type ,size)
       (satisfies even-size-p)))

(deftest deftype-1
    (typep 1 '(even-array integer (10)))
  nil)

(deftest deftype-2
    (typep nil '(even-array t (*)))
  nil)

(deftest deftype-3
    (not (not (typep (make-array '(10)) '(even-array t (*)))))
  t)

(deftest deftype-4
    (typep (make-array '(5)) '(even-array t (*)))
  nil)

(deftest deftype-5
    (not (not (typep (make-string 10) '(even-array character (*)))))
  t)

(deftest deftype-6
    (not (not
	  (typep (make-array '(3 5 6) :element-type '(unsigned-byte 8))
	   '(even-array (unsigned-byte 8)))))
  t)

;; This should be greatly expanded

