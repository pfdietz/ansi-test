;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Mar 19 21:48:39 1998
;;;; Contains: Data for testing type and class inclusions

;; We should check for every type that NIL is a subtype, and T a supertype

(in-package :cl-test)
(use-package :rt)

(declaim (optimize (safety 3)))

(deftest boolean-type-1
  (notnot-mv (typep nil 'boolean))
  t)

(deftest boolean-type-2
  (notnot-mv (typep t 'boolean))
  t)

(deftest boolean-type-3
  (check-type-predicate 'is-t-or-nil 'boolean)
  0)

;; Two type inclusions on booleans
;; have been conditionalized to prevent
;; some tests from doing too badly on CMU CL on x86
;; These should get removed when I get a more up to date
;; image for that platform -- pfd

(defparameter *subtype-table*
(let ((table
       '(
	 (null symbol)
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
	 )))
  (when (subtypep* 'character 'base-char)
    (setq table
	  (append
	   '((character base-char)
	     (string base-string)
	     (simple-string simple-base-string))
	   table)))
  
  table))

(deftest types-3
    (count-if
     #'(lambda (pair)
	 (let ((t1 (first pair))
	       (t2 (second pair)))
	   (cond
	    ((not (subtypep* t1 t2))
	     (format t "~%Problem!  Did not find ~S to be a subtype of ~S" t1 t2)
	     t)
	    (t nil))))
     *subtype-table*)
  0)

(defparameter +float-types+
  '(long-float double-float short-float single-float))


;;; This next test is all screwed up.  Basically, it assumes
;;; incorrectly that certain subtype relationships that are
;;; not specified in the spec cannot occur.
#|
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
		   ((and (not (eqt tp tp2))
			 (not (eqt tp2 'standard-object))
			 (not (eqt tp2 'structure-object))
			 (not (member tp2 parents))
			 (subtypep* tp tp2)
			 (not (and (member tp +float-types+)
				   (member tp2 +float-types+)))
			 (not (and (eqt tp2 'structure-object)
				   (member 'standard-object parents))))
		    (format t "~%Improper subtype: ~S of ~S"
			    tp tp2)
		    1)
		   (t 0)))))
    ))

(deftest types-4
    (types-4-body)
  0)
|#

(deftest types-6
    (types-6-body)
  0)

(declaim (special *disjoint-types-list*))

(deftest types-7
    (loop
	for tp in *disjoint-types-list* sum
	  (loop for tp2 in *disjoint-types-list* count
		(and (not (eqt tp tp2))
		     (subtypep* tp tp2))))
  0)

(deftest types-8
    (loop
	for tp in *disjoint-types-list* count
	  (cond
	   ((and (not (eqt tp 'cons))
		 (not (subtypep* tp 'atom)))
	    (format t "~%Should be atomic, but isn't: ~S" tp)
	    t)))
  0)

(declaim (special *type-list*
		  *supertype-table*))

;;;
;;; TYPES-9 checks the transitivity of SUBTYPEP on pairs of types
;;; occuring in *SUBTYPE-TABLE*, as well as the types KEYWORD, ATOM,
;;; and LIST (the relationships given in *SUBTYPE-TABLE* are not used
;;; here.)
;;;

(deftest types-9
    (types-9-body)
  nil)

;;;
;;; TYPES-9A takes the supertype relationship computed by test TYPE-9
;;; and checks that TYPEP respects it for all elements of *UNIVERSE*.
;;; That is, if T1 and T2 are two types, and X is an element of *UNIVERSE*,
;;; then if (SUBTYPEP T1) then (TYPEP X T1) implies (TYPEP X T2).
;;;
;;; The function prints error messages when this fails, and returns the
;;; number of occurences of failure.
;;;
;;; Test TYPES-9 must be run before this test.
;;;

(deftest types-9a
    (types-9a-body)
  0)

;;;
;;; Check that disjointness of types in *disjoint-types-list*
;;; is respected by all the elements of *universe*
;;;
(deftest universe-elements-in-at-most-one-disjoint-type
  (loop for e in *universe*
	for types = (remove-if-not #'(lambda (x) (typep e x))
				   *disjoint-types-list*)
	when (> (length types) 1)
	collect (list e types))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; deftype

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
  (notnot-mv (typep (make-array '(10)) '(even-array t (*))))
  t)

(deftest deftype-4
  (typep (make-array '(5)) '(even-array t (*)))
  nil)

(deftest deftype-5
  (notnot-mv (typep (make-string 10) '(even-array character (*))))
  t)

(deftest deftype-6
  (notnot-mv
   (typep (make-array '(3 5 6) :element-type '(unsigned-byte 8))
	  '(even-array (unsigned-byte 8))))
  t)

;; This should be greatly expanded

(defparameter *type-and-class-fns*
  '(coerce subtypep type-of typep type-error-datum type-error-expected-type))

(deftest type-and-class-fns
  (remove-if #'fboundp *type-and-class-fns*)
  nil)

(deftest type-and-class-macros
  (notnot-mv (macro-function 'deftype))
  t)

(deftest typep-nil-null
  (notnot-mv (typep nil 'null))
  t)

(deftest typep-t-null
  (typep t 'null)
  nil)


;;; Error checking of type-related functions

(deftest type-of.error.1
  (classify-error (type-of))
  program-error)

(deftest type-of.error.2
  (classify-error (type-of nil nil))
  program-error)

(deftest typep.error.1
  (classify-error (typep))
  program-error)

(deftest typep.error.2
  (classify-error (typep nil))
  program-error)

(deftest typep.error.3
  (classify-error (typep nil t nil nil))
  program-error)

(deftest type-error-datum.error.1
  (classify-error (type-error-datum))
  program-error)

(deftest type-error-datum.error.2
  (classify-error
   (let ((c (make-condition 'type-error :datum nil
			    :expected-type t)))
     (type-error-datum c nil)))
  program-error)

(deftest type-error-expected-type.error.1
  (classify-error (type-error-expected-type))
  program-error)

(deftest type-error-expected-type.error.2
  (classify-error
   (let ((c (make-condition 'type-error :datum nil
			    :expected-type t)))
     (type-error-expected-type c nil)))
  program-error)

;;; Tests of env arguments to typep, subtypep

(deftest typep.env.1
  (notnot-mv (typep 0 'bit nil))
  t)

(deftest typep.env.2
  (macrolet ((%foo (&environment env)
		   (notnot-mv (typep 0 'bit env))))
    (%foo))
  t)

(deftest typep.env.3
  (macrolet ((%foo (&environment env)
		   (notnot-mv (typep env (type-of env)))))
    (%foo))
  t)

