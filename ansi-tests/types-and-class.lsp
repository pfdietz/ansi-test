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



(deftest types-3
  (loop
   for (t1 t2) in *subtype-table*
   for m1 = (check-subtypep t1 t2 t t)
   for m2 = (check-subtypep `(and ,t1 ,t2) t1 t)
   for m3 = (check-subtypep `(and ,t2 ,t1) t1 t)
   for m4 = (check-subtypep `(and ,t1 (not ,t2)) nil t)
   for m5 = (check-subtypep `(and (not ,t2) ,t1) nil t)
   when m1 collect m1
   when m2 collect m2
   when m3 collect m3
   when m4 collect m4
   when m5 collect m5)
  nil)

(declaim (special +float-types+ *subtype-table*))

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
  nil)

(declaim (special *disjoint-types-list*))

;;; Check that the disjoint types really are disjoint

(deftest types-7b
  (loop for e on *disjoint-types-list*
	for tp1 = (first e)
	append
	(loop for tp2 in (rest e)
	      append (classes-are-disjoint tp1 tp2)))
  nil)

(deftest types-7c
  (loop for e on *disjoint-types-list2*
	for list1 = (first e)
	append
	(loop for tp1 in list1 append
	      (loop for list2 in (rest e)
		    append
		    (loop for tp2 in list2 append
			  (classes-are-disjoint tp1 tp2)))))
  nil)

(deftest types-8
  (loop
   for tp in *disjoint-types-list* count
   (cond
    ((and (not (eqt tp 'cons))
	  (not (subtypep* tp 'atom)))
     (format t "~%Should be atomic, but isn't: ~S" tp)
     t)))
  0)

(declaim (special *type-list* *supertype-table*))

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

;;; Union of a type with its complement is universal

(deftest type-or-not-type-is-everything
  (loop for l in *disjoint-types-list2*
	append
	(loop
	 for type in l
	 append (check-subtypep t `(or ,type (not ,type)) t)
	 append (check-subtypep t `(or (not ,type) ,type) t)))
  nil)

(defclass tac-1-class () (a b c))
(defclass tac-1a-class (tac-1-class) (d e))
(defclass tac-1b-class (tac-1-class) (f g))

(deftest user-class-disjointness
  (loop for l in *disjoint-types-list2*
	append
	(loop
	 for type in l
	 append (classes-are-disjoint type 'tac-1-class)))
  nil)

(deftest user-class-disjointness-2
  (check-disjointness 'tac-1a-class 'tac-1b-class)
  nil)

(defstruct tac-2-struct a b c)
(defstruct (tac-2a-struct (:include tac-2-struct)) d e)
(defstruct (tac-2b-struct (:include tac-2-struct)) f g)

(deftest user-struct-disjointness
  (loop for l in *disjoint-types-list2*
	append
	(loop
	 for type in l
	 append (check-disjointness type 'tac-2-struct)))
  nil)

(deftest user-struct-disjointness-2
  (check-disjointness 'tac-2a-struct 'tac-2b-struct)
  nil)

(deftest integer-and-ratio-are-disjoint
  (classes-are-disjoint 'integer 'ratio)
  nil)

(deftest bignum-and-ratio-are-disjoint
  (classes-are-disjoint 'bignum 'ratio)
  nil)

(deftest bignum-and-fixnum-are-disjoint
  (classes-are-disjoint 'bignum 'fixnum)
  nil)

(deftest fixnum-and-ratio-are-disjoint
  (classes-are-disjoint 'fixnum 'ratio)
  nil)

(deftest byte8-and-ratio-are-disjoint
  (classes-are-disjoint '(unsigned-byte 8) 'ratio)
  nil)

(deftest bit-and-ratio-are-disjoint
  (classes-are-disjoint 'bit 'ratio)
  nil)

(deftest integer-and-float-are-disjoint
  (classes-are-disjoint 'integer 'float)
  nil)

(deftest ratio-and-float-are-disjoint
  (classes-are-disjoint 'ratio 'float)
  nil)

(deftest complex-and-float-are-disjoint
  (classes-are-disjoint 'complex 'float)
  nil)

(deftest integer-subranges-are-disjoint
  (classes-are-disjoint '(integer 0 (10)) '(integer 10 (20)))
  nil)

(deftest keyword-and-null-are-disjoint
  (classes-are-disjoint 'keyword 'null)
  nil)

(deftest keyword-and-boolean-are-disjoint
  (classes-are-disjoint 'keyword 'boolean)
  nil)


;;; Check that all class names in CL that name standard-classes or
;;; structure-classes are subtypes of standard-object and structure-object,
;;; respectively

(deftest all-standard-classes-are-subtypes-of-standard-object
  (loop for sym being  the external-symbols of "COMMON-LISP"
	for class = (find-class sym nil)
	when (and class
		  (typep class 'standard-class)
		  (or (not (subtypep sym 'standard-object))
		      (not (subtypep class 'standard-object))))
	collect sym)
  nil)

(deftest all-structure-classes-are-subtypes-of-structure-object
  (loop for sym being the external-symbols of "COMMON-LISP"
	for class = (find-class sym nil)
	when (and class
		  (typep class 'structure-class)
		  (or (not (subtypep sym 'structure-object))
		      (not (subtypep class 'structure-object))))
	collect sym)
  nil)
		  
;;; Confirm that only the symbols exported from CL that are supposed
;;; to be types are actually classes (see section 11.1.2.1.1)

(deftest all-exported-cl-class-names-are-valid
  (loop for sym being the external-symbols of "COMMON-LISP"
	when (and (find-class sym nil)
		  (not (member sym *cl-all-type-symbols* :test #'eq)))
	collect sym)
  nil)

;;; Confirm that all standard generic functions are instances of
;;; the class standard-generic-function.

(deftest all-standard-generic-functions-are-instances-of-that-class
  (loop for sym in *cl-standard-generic-function-symbols*
	for fun = (and (fboundp sym) (symbol-function sym))
	unless (and (typep fun 'generic-function)
		    (typep fun 'standard-generic-function))
	collect (list sym fun))
  nil)

;;; Canonical metaobjects are in the right classes

(deftest structure-object-is-in-structure-class
  (notnot-mv (typep (find-class 'structure-object) 'structure-class))
  t)

(deftest standard-object-is-in-standard-class
  (notnot-mv (typep (find-class 'standard-object) 'standard-class))
  t)


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

