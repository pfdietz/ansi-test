;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 28 17:10:18 1998
;;;; Contains: Aux. functions for CL-TEST

(in-package :cl-test)

(declaim (optimize (safety 3)))

;;; A function for coercing truth values to BOOLEAN

(defun notnot (x) (not (not x)))

(defmacro notnot-mv (form)
  `(notnot-mv-fn (multiple-value-list ,form)))

(defun notnot-mv-fn (results)
  (if (null results)
      (values)
    (apply #'values
	   (not (not (first results)))
	   (rest results))))

(defmacro not-mv (form)
  `(not-mv-fn (multiple-value-list ,form)))

(defun not-mv-fn (results)
  (if (null results)
      (values)
    (apply #'values
	   (not (first results))
	   (rest results))))


;;; Macro to check that a function is returning a specified number of values
;;; (defaults to 1)
(defmacro check-values (form &optional (num 1))
  (let ((v (gensym))
	(n (gensym)))
   `(let ((,v (multiple-value-list ,form))
	  (,n ,num))
      (check-values-length ,v ,n ',form)
      (car ,v))))

(defun check-values-length (results expected-number form)
  (declare (type fixnum expected-number))
  (let ((n expected-number))
    (declare (type fixnum n))
    (dolist (e results)
      (declare (ignore e))
      (decf n))
    (unless (= n 0)
      (error "Expected ~A results from ~A, got ~A results instead.~%~
Results: ~A~%" expected-number form n results))))

;;; Do multiple-value-bind, but check # of arguments
(defmacro multiple-value-bind* ((&rest vars) form &body body)
  (let ((len (length vars))
	(v (gensym)))
    `(let ((,v (multiple-value-list ,form)))
       (check-values-length ,v ,len ',form)
       (destructuring-bind ,vars ,v ,@body))))
  
;;; Comparison functions that are like various builtins,
;;; but are guaranteed to return T for true.

(defun eqt (x y)
  "Like EQ, but guaranteed to return T for true."
  (apply #'values (mapcar #'notnot (multiple-value-list (eq x y)))))

(defun eqlt (x y)
  "Like EQL, but guaranteed to return T for true."
  (apply #'values (mapcar #'notnot (multiple-value-list (eql x y)))))

(defun equalt (x y)
  "Like EQUAL, but guaranteed to return T for true."
  (apply #'values (mapcar #'notnot (multiple-value-list (equal x y)))))

(defun equalpt (x y)
  "Like EQUALP, but guaranteed to return T for true."
  (apply #'values (mapcar #'notnot (multiple-value-list (equalp x y)))))

(defun =t (x &rest args)
  "Like =, but guaranteed to return T for true."
  (apply #'values (mapcar #'notnot (multiple-value-list (apply #'=  x args)))))

(defun make-int-list (n)
  (loop for i from 0 below n collect i))

(defun make-int-array (n &optional (fn #'make-array))
  (let ((a (funcall fn n)))
    (loop for i from 0 below n do (setf (aref a i) i))
    a))

;;; Return true if A1 and A2 are arrays with the same rank
;;; and dimensions whose elements are EQUAL

(defun equal-array (a1 a2)
  (and (typep a1 'array)
       (typep a2 'array)
       (= (array-rank a1) (array-rank a2))
       (if (= (array-rank a1) 0)
	   (equal (aref a1) (aref a2))
	 (let ((ad (array-dimensions a1)))
	   (and (equal ad (array-dimensions a2))
		(if (= (array-rank a1) 1)
		    (let ((as (first ad)))
		      (loop
		       for i from 0 below as
		       always (equal (aref a1 i) (aref a2 i))))
		  (let ((as (array-total-size a1)))
		    (and (= as (array-total-size a2))
			 (loop
			  for i from 0 below as
			  always (equal (row-major-aref a1 i)
					(row-major-aref a2 i)))))))))))

;;; *universe* is defined elsewhere -- it is a list of various
;;; lisp objects used when stimulating things in various tests.
(declaim (special *universe*))

;;; The function EMPIRICAL-SUBTYPEP checks two types
;;; for subtypeness, first using SUBTYPEP*, then (if that
;;; fails) empirically against all the elements of *universe*,
;;; checking if all that are in the first are also in the second.
;;; Return T if this is the case, NIL otherwise.  This will
;;; always return T if type1 is truly a subtype of type2,
;;; but may return T even if this is not the case.

(defun empirical-subtypep (type1 type2)
  (multiple-value-bind (sub good)
      (subtypep* type1 type2)
    (if good
	sub
      (loop for e in *universe*
	    always (or (not (typep e type1)) (typep e type2))))))

;;; Check that the subtype relationships implied
;;; by disjointness are not contradicted.  Return NIL
;;; if ok, or a list of error messages if not.

;;; Assumes the types are nonempty.

(defun check-disjointness (type1 type2)
  (append
   (check-subtypep type1 type2 nil)
   (check-subtypep type2 type1 nil)
   (check-subtypep type1 `(not ,type2) t)
   (check-subtypep type2 `(not ,type1) t)
   (check-subtypep `(and ,type1 ,type2) nil t)
   (check-subtypep `(and ,type2 ,type1) nil t)
   (check-subtypep `(and ,type1 (not ,type2)) type1 t)
   (check-subtypep `(and (not ,type2) ,type1) type1 t)
   (check-subtypep `(and ,type2 (not ,type1)) type2 t)
   (check-subtypep `(and (not ,type1) ,type2) type2 t)
;;;   (check-subtypep type1 `(or ,type1 (not ,type2)) t)
;;;   (check-subtypep type1 `(or (not ,type2) ,type1) t)
;;;   (check-subtypep type2 `(or ,type2 (not ,type1)) t)
;;;   (check-subtypep type2 `(or (not ,type1) ,type2) t)
   (check-subtypep t `(or (not ,type1) (not ,type2)) t)
   (check-subtypep t `(or (not ,type2) (not ,type1)) t)
   ))

(defun check-equivalence (type1 type2)
  (append
   (check-subtypep type1 type2 t)
   (check-subtypep type2 type1 t)
   (check-subtypep `(not ,type1) `(not ,type2) t)
   (check-subtypep `(not ,type2) `(not ,type1) t)
   (check-subtypep `(and ,type1 (not ,type2)) nil t)
   (check-subtypep `(and ,type2 (not ,type1)) nil t)
   (check-subtypep `(and (not ,type2) ,type1) nil t)
   (check-subtypep `(and (not ,type1) ,type2) nil t)
   (check-subtypep t `(or ,type1 (not ,type2)) t)
   (check-subtypep t `(or ,type2 (not ,type1)) t)
   (check-subtypep t `(or (not ,type2) ,type1) t)
   (check-subtypep t `(or (not ,type1) ,type2) t)))

(defun check-all-subtypep (type1 type2)
  (append
   (check-subtypep type1 type2 t)
   (check-subtypep `(not ,type2) `(not ,type1) t)
   (check-subtypep `(and ,type1 (not ,type2)) nil t)
   (check-subtypep t `(or (not ,type1) ,type2) t)))   

(defun check-all-not-subtypep (type1 type2)
  (append
   (check-subtypep type1 type2 nil)
   (check-subtypep `(not ,type2) `(not ,type1) nil)))

(defun check-subtypep (type1 type2 is-sub &optional should-be-valid)
  (multiple-value-bind
      (sub valid)
      (subtypep type1 type2)
    (unless (constantp type1) (setq type1 (list 'quote type1)))
    (unless (constantp type2) (setq type2 (list 'quote type2)))
    (if (or (and valid sub (not is-sub))
	    (and valid (not sub) is-sub)
	    (and (not valid) should-be-valid))
	`(((SUBTYPEP ,type1 ,type2) cl-user::==> ,sub ,valid))
      nil)))

(defun check-type-predicate (P TYPE)
  "Check that a predicate P is the same as #'(lambda (x) (typep x TYPE))
   by applying both to all elements of *UNIVERSE*.  Print message
   when a mismatch is found, and return number of mistakes."
  (loop
      for x in *universe* count
	(block failed
	  (let ((p1 (handler-case
			(funcall P x)
		      (error () (format t "(FUNCALL ~S ~S) failed~%"
					P x)
			(return-from failed t))))
		(p2 (handler-case
			(typep x TYPE)
		      (error () (format t "(TYPEP ~S '~S) failed~%"
					x TYPE)
			(return-from failed t)))))
	      (when (or (and p1 (not p2))
			(and (not p1) p2))
		(format t "(FUNCALL ~S ~S) = ~S, (TYPEP ~S '~S) = ~S~%"
			P x p1 x TYPE p2)
		t)))))

(declaim (special *catch-error-type*))

(defun catch-continue-debugger-hook (condition dbh)
  "Function that when used as *debugger-hook*, causes
   continuable errors to be continued without user intervention."
  (declare (ignore dbh))
  (let ((r (find-restart 'continue condition)))
    (cond
     ((and *catch-error-type*
	   (not (typep condition *catch-error-type*)))
      (format t "Condition ~S is not a ~A~%" condition *catch-error-type*)
      (cond (r (format t "Its continue restart is ~S~%" r))
	    (t (format t "It has no continue restart~%")))
      (throw 'continue-failed nil))
     (r (invoke-restart r))
     (t (throw 'continue-failed nil)))))

#|
(defun safe (fn &rest args)
  "Apply fn to args, trapping errors.  Convert type-errors to the
   symbol type-error."
  (declare (optimize (safety 3)))
  (handler-case
   (apply fn args)
   (type-error () 'type-error)
   (error (c) c)))
|#

;;; Use the next macro in place of SAFE

(defmacro catch-type-error (form)
"Evaluate form in safe mode, returning its value if there is no error.
If an error does occur, return type-error on TYPE-ERRORs, or the error
condition itself on other errors."
`(locally (declare (optimize (safety 3)))
  (handler-case ,form
     (type-error () 'type-error)
     (error (c) c))))

(defmacro classify-error* (form)
"Evaluate form in safe mode, returning its value if there is no error.
If an error does occur, return a symbol classify the error, or allow
the condition to go uncaught if it cannot be classified."
`(locally (declare (optimize (safety 3)))
  (handler-case ,form
     (undefined-function () 'undefined-function)
     (program-error () 'program-error)
     (package-error () 'package-error)
     (type-error    () 'type-error)
     (control-error () 'control-error)
     (stream-error  () 'stream-error)
     (reader-error  () 'reader-error)
     (file-error    () 'file-error)
     (control-error () 'control-error)
     (cell-error    () 'cell-error)
     (error         () 'error)
  )))

(defun classify-error** (form)
  (handler-bind ((warning #'(lambda (c) (declare (ignore c))
			      (muffle-warning))))
		(proclaim '(optimize (safety 3)))
		(classify-error*
		 (if regression-test::*compile-tests*
		     (funcall (compile nil `(lambda ()
					      (declare (optimize (safety 3)))
					      ,form)))
		     (eval form))
		 )))

(defmacro classify-error (form)
  `(classify-error** ',form))

;;;
;;; A scaffold is a structure that is used to remember the object
;;; identities of the cons cells in a (noncircular) data structure.
;;; This lets us check if the data structure has been changed by
;;; an operation.
;;;

(defstruct scaffold
  node
  car
  cdr)

(defun make-scaffold-copy (x)
  "Make a tree that will be used to check if a tree has been changed."
  (if
      (consp x)
      (make-scaffold :node x
		     :car (make-scaffold-copy (car x))
		     :cdr (make-scaffold-copy (cdr x)))
    (make-scaffold :node x
		   :car nil
		   :cdr nil)))

(defun check-scaffold-copy (x xcopy)
  "Return t if xcopy were produced from x by make-scaffold-copy,
   and none of the cons cells in the tree rooted at x have been
   changed."

  (and (eq x (scaffold-node xcopy))
       (or
	(not (consp x))
	(and
	 (check-scaffold-copy (car x) (scaffold-car xcopy))
	 (check-scaffold-copy (cdr x) (scaffold-cdr xcopy))))))

(defun create-c*r-test (n)
  (cond
   ((<= n 0) 'none)
   (t
    (cons (create-c*r-test (1- n))
	  (create-c*r-test (1- n))))))

(defun nth-1-body (x)
  (loop
      for e in x
       and i from 0
       count (not (eqt e (nth i x)))))

;;;
;;; The function SUBTYPEP should return two generalized booleans.
;;; This auxiliary function returns booleans instead
;;; (which makes it easier to write tests).
;;;
(defun subtypep* (type1 type2)
  (apply #'values
	 (mapcar #'notnot
		 (multiple-value-list (subtypep type1 type2)))))

(defun subtypep*-or-fail (type1 type2)
  (let ((results (multiple-value-list (subtypep type1 type2))))
    (and (= (length results) 2)
	 (or (not (second results))
	     (notnot (first results))))))

(defun subtypep*-not-or-fail (type1 type2)
  (let ((results (multiple-value-list (subtypep type1 type2))))
    (and (= (length results) 2)
	 (or (not (second results))
	     (not (first results))))))

;;; (eval-when (load eval compile)
;;;   (unless (fboundp 'complement)
;;;     (defun complement (fn)
;;;       #'(lambda (&rest args) (not (apply fn args))))))

(defun compose (&rest fns)
  (let ((rfns (reverse fns)))
    #'(lambda (x) (loop for f in rfns do (setf x (funcall f x))) x)))

(defun evendigitp (c)
  (notnot (find c "02468")))

(defun odddigitp (c)
  (notnot (find c "13579")))

(defun nextdigit (c)
  (cadr (member c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))))

(defun is-eq-p (x) #'(lambda (y) (eqt x y)))
(defun is-not-eq-p (x) #'(lambda (y) (not (eqt x y))))

(defun is-eql-p (x) #'(lambda (y) (eqlt x y)))
(defun is-not-eql-p (x) #'(lambda (y) (not (eqlt x y))))

(defun onep (x) (eql x 1))

(defun char-invertcase (c)
  (if (upper-case-p c) (char-downcase c)
    (char-upcase c)))

(defun string-invertcase (s)
  (map 'string #'char-invertcase s))

(defun symbol< (x &rest args)
  (apply #'string< (symbol-name x) (mapcar #'symbol-name args)))

(defun random-from-seq (seq)
  "Generate a random member of a sequence."
  (let ((len (length seq)))
    (assert (> len 0))
    (elt seq (random len))))

(defmacro random-case (&body cases)
  (let ((len (length cases)))
    (assert (> len 0))
    `(case (random ,len)
       ,@(loop for i from 0 for e in cases collect `(,i ,e))
       (t (error "Can't happen?! (in random-case~%")))))

(defun coin (&optional (n 2))
  "Flip an n-sided coin."
  (eql (random n) 0))

;;; Randomly permute a sequence
(defun random-permute (seq)
  (setq seq (copy-seq seq))
  (let ((len (length seq)))
    (loop for i from len downto 2
	  do (let ((r (random i)))
	       (rotatef (elt seq r) (elt seq (1- i))))))
  seq)

(defun make-list-expr (args)
  "Build an expression for computing (LIST . args), but that evades
   CALL-ARGUMENTS-LIMIT."
  (if (cddddr args)
      (list 'list*
	    (first args) (second args) (third args) (fourth args)
	    (make-list-expr (cddddr args)))
    (cons 'list args)))  

(defparameter +standard-chars+
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789~!@#$%^&*()_+|\\=-`{}[]:\";'<>?,./
 ")

(defparameter
  +base-chars+ #.(concatenate 'string
			      "abcdefghijklmnopqrstuvwxyz"
			      "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
			      "0123456789"
			      "<,>.?/\"':;[{]}~`!@#$%^&*()_-+= \\|"))

(defparameter +num-base-chars+ (length +base-chars+))


(defparameter +alpha-chars+ (subseq +standard-chars+ 0 52))
(defparameter +lower-case-chars+ (subseq +alpha-chars+ 0 26))
(defparameter +upper-case-chars+ (subseq +alpha-chars+ 26 52))
(defparameter +alphanumeric-chars+ (subseq +standard-chars+ 0 62))
(defparameter +digit-chars+ "0123456789")
(defparameter +extended-digit-chars+ "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(defparameter +code-chars+
  (coerce (loop for i from 0 below 256
		for c = (code-char i)
		when c collect c)
	  'string))
(defparameter +rev-code-chars+ (reverse +code-chars+))

;;; Used in checking for continuable errors

(defun has-non-abort-restart (c)
  (throw 'handled
	 (if (position 'abort (compute-restarts c)
		       :key #'restart-name :test-not #'eq)
	     'success
	   'fail)))

(defmacro handle-non-abort-restart (&body body)
  `(catch 'handled
     (handler-bind ((error #'has-non-abort-restart))
		   ,@body)))

;;; used in elt.lsp
(defun elt-v-6-body ()
  (let ((x (make-int-list 1000)))
    (let ((a (make-array '(1000) :initial-contents x)))
      (loop
	  for i from 0 to 999 do
	    (unless (eql i (elt a i)) (return nil))
	  finally (return t)))))

(defun make-adj-array (n &key initial-contents)
  (if initial-contents
      (make-array n :adjustable t :initial-contents initial-contents)
    (make-array n :adjustable t)))

;;; used in elt.lsp
(defun elt-adj-array-6-body ()
  (let ((x (make-int-list 1000)))
    (let ((a (make-adj-array '(1000) :initial-contents x)))
      (loop
	  for i from 0 to 999 do
	    (unless (eql i (elt a i)) (return nil))
	  finally (return t)))))

(defparameter *displaced* (make-int-array 100000))

(defun make-displaced-array (n displacement)
  (make-array n :displaced-to *displaced*

	      :displaced-index-offset displacement))

;;; used in fill.lsp
(defun array-unsigned-byte-fill-test-fn (byte-size &rest fill-args)
  (let* ((a (make-array '(5) :element-type (list 'unsigned-byte byte-size)
			:initial-contents '(1 2 3 4 5)))
	 (b (apply #'fill a fill-args)))
    (values (eqt a b)
	    (map 'list #'identity a))))

;;; used in fill-strings.lsp
(defun array-string-fill-test-fn (a &rest fill-args)
  (setq a (copy-seq a))
  (let ((b (apply #'fill a fill-args)))
    (values (eqt a b) b)))

;;; From types-and-class.lsp

(defparameter +float-types+
  '(long-float double-float short-float single-float))

(defparameter *subtype-table*
(let ((table
       '(
	 (null symbol)
	 (symbol t)
	 (boolean symbol)
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
	 (null boolean)
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

(defparameter *disjoint-types-list*
    '(cons symbol array
      number character hash-table function readtable package
      pathname stream random-state condition restart))

(defparameter *disjoint-types-list2*
  `((cons (cons t t) (cons t (cons t t)) (eql (nil)))
    (symbol keyword boolean null (eql a) (eql nil) (eql t) (eql *))
    (array vector simple-array simple-vector string simple-string
	   base-string simple-base-string (eql #()))
    (character base-char standard-char (eql #\a)
	       ,@(if (subtypep 'character 'base-char) nil
		   (list 'extended-char)))
    (function compiled-function generic-function standard-generic-function
	      (eql ,#'car))
    (package (eql ,(find-package "COMMON-LISP")))
    (pathname logical-pathname (eql #p""))
    (stream broadcast-stream concatenated-stream echo-stream
	    file-stream string-stream synonym-stream two-way-stream)
    (number real complex float integer rational ratio fixnum
	    bit (integer 0 100) (float 0.0 100.0) (integer 0 *)
	    (rational 0 *) (mod 10)
	    (eql 0)
	    ,@(and (not (subtypep 'bignum nil))
		   (list 'bignum)))
    (random-state)
    ,*condition-types*
    (restart)
    (readtable)))

(defparameter *types-list3*
  (reduce #'append *disjoint-types-list2* :from-end t))

(defun trim-list (list n)
  (let ((len (length list)))
    (if (<= len n) list
      (append (subseq list 0 n)
	      (format nil "And ~A more omitted." (- len n))))))

(defun is-t-or-nil (e)
  (or (eqt e t) (eqt e nil)))

(defun is-builtin-class (type)
  (when (symbolp type) (setq type (find-class type nil)))
  (typep type 'built-in-class))

(defun classes-are-disjoint (c1 c2)
  "If either c1 or c2 is a builtin class or the name of a builtin
   class, then check for disjointness.  Return a non-NIL list
   of failed subtypep relationships, if any."
  (and (or (is-builtin-class c1)
	   (is-builtin-class c2))
       (check-disjointness c1 c2)))

(declaim (special *subtype-table*))

(defun types.6-body ()
  (loop
      for p in *subtype-table*
      for tp = (car p)
      append
      (and (not (member tp '(sequence cons list t)))
	   (let ((message (check-subtypep tp 'atom t t)))
	     (if message (list message))))))

(defparameter *type-list* nil)
(defparameter *supertype-table* nil)
(declaim (special *subtype-table*))

(defun types.9-body ()
  (let ((tp-list (append '(keyword atom list)
			 (loop for p in *subtype-table* collect (car p))))
	(result-list))
    (setf tp-list (remove-duplicates tp-list))
    ;; TP-LIST is now a list of unique CL type names
    ;; Store it in *TYPE-LIST* so we can inspect it later if this test
    ;; fails.  The variable is also used in test TYPES.9A
    (setf *type-list* tp-list)
    ;; Compute all pairwise SUBTYPEP relationships among
    ;; the elements of *TYPE-LIST*.
    (let ((subs (make-hash-table :test #'eq))
	  (sups (make-hash-table :test #'eq)))
      (loop
	  for x in tp-list do
	    (loop
		for y in tp-list do
		  (multiple-value-bind (result good)
		      (subtypep* x y)
		    (declare (ignore good))
		    (when result
		      (pushnew x (gethash y subs))
		      (pushnew y (gethash x sups))))))
      ;; Store the supertype relations for later inspection
      ;; and use in test TYPES.9A
      (setf *supertype-table* sups)
      ;; Check that the relation we just computed is transitive.
      ;; Return a list of triples on which transitivity fails.
      (loop
	  for x in tp-list do
	    (let ((sub-list (gethash x subs))
		  (sup-list (gethash x sups)))
	      (loop
		  for t1 in sub-list do
		    (loop
			for t2 in sup-list do
			  (multiple-value-bind (result good)
			      (subtypep* t1 t2)
			    (when (and good (not result))
			      (pushnew (list t1 x t2) result-list
				       :test #'equal)))))))
      
      result-list)))

;;; TYPES.9-BODY returns a list of triples (T1 T2 T3)
;;; where (AND (SUBTYPEP T1 T2) (SUBTYPEP T2 T3) (NOT (SUBTYPEP T1 T3)))
;;;  (and where SUBTYPEP succeeds in each case, returning true as its
;;;   second return value.)

(defun types.9a-body ()
  (cond
   ((not (and *type-list* *supertype-table*))
    (format nil "Run test type.9 first~%")
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
		    (format t "Found element of ~S not in ~S: ~S~%"
			    tp tp2 x)
		    t))
	     (condition (c) (format t "Error ~S occured: ~S~%"
				    c tp2)
			t)))))
	 (condition (c) (format t "Error ~S occured: ~S~%" c tp)
		    1))))))))

(defun even-size-p (a)
  (some #'evenp (array-dimensions a)))

(defun check-cons-copy (x y)
  "Check that the tree x is a copy of the tree y,
   returning t if it is, nil if not."
  (cond
   ((consp x)
    (and (consp y)
	 (not (eqt x y))
	 (check-cons-copy (car x) (car y))
	 (check-cons-copy (cdr x) (cdr y))))
   ((eqt x y) t)
   (t nil)))

(defun check-sublis (a al &key (key 'no-key) test test-not)
  "Apply sublis al a with various keys.  Check that
   the arguments are not themselves changed.  Return nil
   if the arguments do get changed."
  (setf a (copy-tree a))
  (setf al (copy-tree al))
  (let ((acopy (make-scaffold-copy a))
	(alcopy (make-scaffold-copy al)))
    (let ((as
	   (apply #'sublis al a
		  `(,@(when test `(:test ,test))
		    ,@(when test-not `(:test-not ,test-not))
		    ,@(unless (eqt key 'no-key) `(:key ,key))))))
      (and
       (check-scaffold-copy a acopy)
       (check-scaffold-copy al alcopy)
       as))))

(defun check-nsublis (a al &key (key 'no-key) test test-not)
  "Apply nsublis al a, copying these arguments first."
  (setf a (copy-tree a))
  (setf al (copy-tree al))
  (let ((as
	 (apply #'sublis (copy-tree al) (copy-tree a)
		`(,@(when test `(:test ,test))
		    ,@(when test-not `(:test-not ,test-not))
		    ,@(unless (eqt key 'no-key) `(:key ,key))))))
    as))

(defun check-subst (new old tree &key (key 'no-key) test test-not)
  "Call subst new old tree, with keyword arguments if present.
   Check that the arguments are not changed."
  (setf new (copy-tree new))
  (setf old (copy-tree old))
  (setf tree (copy-tree tree))
  (let ((newcopy (make-scaffold-copy new))
	(oldcopy (make-scaffold-copy old))
	(treecopy (make-scaffold-copy tree)))
    (let ((result
	   (apply #'subst new old tree
		  `(,@(unless (eqt key 'no-key) `(:key ,key))
		    ,@(when test `(:test ,test))
		    ,@(when test-not `(:test-not ,test-not))))))
      (and (check-scaffold-copy new newcopy)
	   (check-scaffold-copy old oldcopy)
	   (check-scaffold-copy tree treecopy)
	   result))))


(defun check-subst-if (new pred tree &key (key 'no-key))
  "Call subst-if new pred tree, with various keyword arguments
   if present.  Check that the arguments are not changed."
  (setf new (copy-tree new))
  (setf tree (copy-tree tree))
  (let ((newcopy (make-scaffold-copy new))
	(predcopy (make-scaffold-copy pred))
	(treecopy (make-scaffold-copy tree)))
    (let ((result
	   (apply #'subst-if new pred tree
		  (unless (eqt key 'no-key) `(:key ,key)))))
      (and (check-scaffold-copy new newcopy)
	   (check-scaffold-copy pred predcopy)
	   (check-scaffold-copy tree treecopy)
	   result))))

(defun check-subst-if-not (new pred tree &key (key 'no-key))
  "Call subst-if-not new pred tree, with various keyword arguments
   if present.  Check that the arguments are not changed."
  (setf new (copy-tree new))
  (setf tree (copy-tree tree))
  (let ((newcopy (make-scaffold-copy new))
	(predcopy (make-scaffold-copy pred))
	(treecopy (make-scaffold-copy tree)))
    (let ((result
	   (apply #'subst-if-not new pred tree
		  (unless (eqt key 'no-key) `(:key ,key)))))
      (and (check-scaffold-copy new newcopy)
	   (check-scaffold-copy pred predcopy)
	   (check-scaffold-copy tree treecopy)
	   result))))

(defun check-nsubst (new old tree &key (key 'no-key) test test-not)
  "Call nsubst new old tree, with keyword arguments if present."
  (setf new (copy-tree new))
  (setf old (copy-tree old))
  (setf tree (copy-tree tree))
  (apply #'nsubst new old tree
	 `(,@(unless (eqt key 'no-key) `(:key ,key))
	     ,@(when test `(:test ,test))
	     ,@(when test-not `(:test-not ,test-not)))))

(defun check-nsubst-if (new pred tree &key (key 'no-key))
  "Call nsubst-if new pred tree, with keyword arguments if present."
  (setf new (copy-tree new))
  (setf tree (copy-tree tree))
  (apply #'nsubst-if new pred tree
	 (unless (eqt key 'no-key) `(:key ,key))))

(defun check-nsubst-if-not (new pred tree &key (key 'no-key))
  "Call nsubst-if-not new pred tree, with keyword arguments if present."
  (setf new (copy-tree new))
  (setf tree (copy-tree tree))
  (apply #'nsubst-if-not new pred tree
		  (unless (eqt key 'no-key) `(:key ,key))))

(defun check-copy-list-copy (x y)
  "Check that y is a copy of the list x."
  (if
      (consp x)
      (and (consp y)
	   (not (eqt x y))
	   (eqt (car x) (car y))
	   (check-copy-list-copy (cdr x) (cdr y)))
    (and (eqt x y) t)))

(defun check-copy-list (x)
  "Apply copy-list, checking that it properly copies,
   and checking that it does not change its argument."
  (let ((xcopy (make-scaffold-copy x)))
    (let ((y (copy-list x)))
      (and
       (check-scaffold-copy x xcopy)
       (check-copy-list-copy x y)
       y))))

(defun append-6-body ()
  (let* ((cal (min 2048 call-arguments-limit))
	 (step (max 1 (floor (/ cal) 64))))
    (loop
     for n from 0
     below cal
     by step
     count
     (not
      (equal
       (apply #'append (loop for i from 1 to n
			     collect '(a)))
       (make-list n :initial-element 'a))))))

(defun is-intersection (x y z)
  "Check that z is the intersection of x and y."
  (and
   (listp x)
   (listp y)
   (listp z)
   (loop for e in x
	 always (or (not (member e y))
		    (member e z)))
   (loop for e in y
	 always (or (not (member e x))
		    (member e z)))
   (loop for e in z
	 always (and (member e x) (member e y)))
   t))

(defun shuffle (x)
  (cond
   ((null x) nil)
   ((null (cdr x)) x)
   (t
    (multiple-value-bind
	(y z)
	(split-list x)
      (append (shuffle y) (shuffle z))))))

(defun split-list (x)
  (cond
   ((null x) (values nil nil))
   ((null (cdr x)) (values x nil))
   (t
    (multiple-value-bind
	(y z)
	(split-list (cddr x))
      (values (cons (car x) y) (cons (cadr x) z))))))

(defun intersection-12-body (size niters &optional (maxelem (* 2 size)))
  (let ((state (make-random-state)))
    (loop
     for i from 1 to niters do
     (let ((x (shuffle (loop for j from 1 to size
			     collect (random maxelem state))))
	   (y (shuffle (loop for j from 1 to size
			     collect (random maxelem state)))))
       (let ((z (intersection x y)))
	 (let ((is-good (is-intersection x y z)))
	   (unless is-good (return (values x y z)))))))
    nil))

(defun nintersection-with-check (x y &key test)
  (let ((ycopy (make-scaffold-copy y)))
    (let ((result (if test
		      (nintersection x y :test test)
		    (nintersection x y))))
      (if (check-scaffold-copy y ycopy)
	  result
	'failed))))

(defun nintersection-12-body (size niters &optional (maxelem (* 2 size)))
  (let ((state (make-random-state t)))
    (loop
     for i from 1 to niters do
     (let ((x (shuffle (loop for j from 1 to size
			     collect (random maxelem state))))
	   (y (shuffle (loop for j from 1 to size
			     collect (random maxelem state)))))
       (let ((z (nintersection-with-check (copy-list x) y)))
	 (when (eqt z 'failed) (return (values x y z)))
	 (let ((is-good (is-intersection x y z)))
	   (unless is-good (return (values x y z)))))))
    nil))


(defun union-with-check (x y &key test test-not)
  (let ((xcopy (make-scaffold-copy x))
	(ycopy (make-scaffold-copy y)))
    (let ((result (cond
		   (test (union x y :test test))
		   (test-not (union x y :test-not test-not))
		   (t (union x y)))))
      (if (and (check-scaffold-copy x xcopy)
	       (check-scaffold-copy y ycopy))
	  result
	'failed))))

(defun union-with-check-and-key (x y key &key test test-not)
  (let ((xcopy (make-scaffold-copy x))
	(ycopy (make-scaffold-copy y)))
    (let ((result  (cond
		   (test (union x y :key key :test test))
		   (test-not (union x y :key key :test-not test-not))
		   (t (union x y :key key)))))
      (if (and (check-scaffold-copy x xcopy)
	       (check-scaffold-copy y ycopy))
	  result
	'failed))))

(defun check-union (x y z)
  (and (listp x)
       (listp y)
       (listp z)
       (loop for e in z always (or (member e x) (member e y)))
       (loop for e in x always (member e z))
       (loop for e in y always (member e z))
       t))

(defun do-random-unions (size niters &optional (maxelem (* 2 size)))
  (let ((state (make-random-state)))
    (loop
       for i from 1 to niters do
	  (let ((x (shuffle (loop for j from 1 to size collect
				  (random maxelem state))))
		(y (shuffle (loop for j from 1 to size collect
				  (random maxelem state)))))
	    (let ((z (union x y)))
	      (let ((is-good (check-union x y z)))
		(unless is-good (return (values x y z)))))))
    nil))

(defun nunion-with-copy (x y &key test test-not)
  (setf x (copy-list x))
  (setf y (copy-list y))
  (cond
   (test (nunion x y :test test))
   (test-not (nunion x y :test-not test-not))
   (t (nunion x y))))

(defun nunion-with-copy-and-key (x y key &key test test-not)
  (setf x (copy-list x))
  (setf y (copy-list y))
  (cond
   (test (nunion x y :key key :test test))
   (test-not (nunion x y :key key :test-not test-not))
   (t (nunion x y :key key))))

(defun do-random-nunions (size niters &optional (maxelem (* 2 size)))
  (let ((state (make-random-state)))
    (loop
       for i from 1 to niters do
	  (let ((x (shuffle (loop for j from 1 to size collect
				  (random maxelem state))))
		(y (shuffle (loop for j from 1 to size collect
				  (random maxelem state)))))
	    (let ((z (nunion-with-copy x y)))
	      (let ((is-good (check-union x y z)))
		(unless is-good (return (values x y z)))))))
    nil))

(defun set-difference-with-check (x y &key (key 'no-key)
					   test test-not)
  (setf x (copy-list x))
  (setf y (copy-list y))
  (let ((xcopy (make-scaffold-copy x))
	(ycopy (make-scaffold-copy y)))
    (let ((result (apply #'set-difference
			 x y
			 `(,@(unless (eqt key 'no-key) `(:key ,key))
			   ,@(when test `(:test ,test))
			   ,@(when test-not `(:test-not ,test-not))))))  
      (cond
       ((and (check-scaffold-copy x xcopy)
	     (check-scaffold-copy y ycopy))
	result)
       (t
	'failed)))))

(defun check-set-difference (x y z &key (key #'identity)
					(test #'eql))
  (and
   ;; (not (eqt 'failed z))
   (listp x)
   (listp y)
   (listp z)
   (loop for e in z always (member e x :key key :test test))
   (loop for e in x always (or (member e y :key key :test test)
			       (member e z :key key :test test)))
   (loop for e in y never  (member e z :key key :test test))
   t))

(defun do-random-set-differences (size niters &optional (maxelem (* 2 size)))
  (let ((state (make-random-state)))
    (loop
     for i from 1 to niters do
     (let ((x (shuffle (loop for j from 1 to size collect
			     (random maxelem state))))
	   (y (shuffle (loop for j from 1 to size collect
			     (random maxelem state)))))
       (let ((z (set-difference-with-check x y)))
	 (let ((is-good (check-set-difference x y z)))
	   (unless is-good (return (values x y z)))))))
    nil))
(defun nset-difference-with-check (x y &key (key 'no-key)
				     test test-not)
  (setf x (copy-list x))
  (setf y (copy-list y))
  (apply #'nset-difference
	 x y
	 `(,@(unless (eqt key 'no-key) `(:key ,key))
	     ,@(when test `(:test ,test))
	     ,@(when test-not `(:test-not ,test-not)))))

(defun check-nset-difference (x y z &key (key #'identity)
				(test #'eql))
  (and
   (listp x)
   (listp y)
   (listp z)
   (loop for e in z always (member e x :key key :test test))
   (loop for e in x always (or (member e y :key key :test test)
			       (member e z :key key :test test)))
   (loop for e in y never  (member e z :key key :test test))
   t))

(defun do-random-nset-differences (size niters &optional (maxelem (* 2 size)))
  (let ((state (make-random-state)))
    (loop
     for i from 1 to niters do
     (let ((x (shuffle (loop for j from 1 to size collect
			     (random maxelem state))))
	   (y (shuffle (loop for j from 1 to size collect
			     (random maxelem state)))))
       (let ((z (nset-difference-with-check x y)))
	 (let ((is-good (check-nset-difference x y z)))
	   (unless is-good (return (values x y z)))))))
    nil))

(defun set-exclusive-or-with-check (x y &key (key 'no-key)
				      test test-not)
  (setf x (copy-list x))
  (setf y (copy-list y))
  (let ((xcopy (make-scaffold-copy x))
	(ycopy (make-scaffold-copy y)))
    (let ((result (apply #'set-exclusive-or
			 x y
			 `(,@(unless (eqt key 'no-key) `(:key ,key))
			     ,@(when test `(:test ,test))
			     ,@(when test-not `(:test-not ,test-not))))))  
      (cond
       ((and (check-scaffold-copy x xcopy)
	     (check-scaffold-copy y ycopy))
	result)
       (t
	'failed)))))

(defun check-set-exclusive-or (x y z &key (key #'identity)
				 (test #'eql))
  (and
   ;; (not (eqt 'failed z))
   (listp x)
   (listp y)
   (listp z)
   (loop for e in z always (or (member e x :key key :test test)
			       (member e y :key key :test test)))
   (loop for e in x always (if (member e y :key key :test test)
			       (not (member e z :key key :test test))
			     (member e z :key key :test test)))
   (loop for e in y always (if (member e x :key key :test test)
			       (not (member e z :key key :test test))
			     (member e z :key key :test test)))
   t))

#|
(defun do-random-set-exclusive-ors (size niters &optional (maxelem (* 2 size)))
  (let ((state (make-random-state)))
    (loop
     for i from 1 to niters do
     (let ((x (shuffle (loop for j from 1 to size collect
			     (random maxelem state))))
	   (y (shuffle (loop for j from 1 to size collect
			     (random maxelem state)))))
       (let ((z (set-exclusive-or-with-check x y)))
	 (let ((is-good (check-set-exclusive-or x y z)))
	   (unless is-good (return (values x y z)))))))
    nil))
|#

(defun nset-exclusive-or-with-check (x y &key (key 'no-key)
				       test test-not)
  (setf x (copy-list x))
  (setf y (copy-list y))
  (apply #'nset-exclusive-or
	 x y
	 `(,@(unless (eqt key 'no-key) `(:key ,key))
	     ,@(when test `(:test ,test))
	     ,@(when test-not `(:test-not ,test-not)))))

#|
(defun do-random-nset-exclusive-ors (size niters &optional (maxelem (* 2 size)))
  (let ((state (make-random-state)))
    (loop
     for i from 1 to niters do
     (let ((x (shuffle (loop for j from 1 to size collect
			     (random maxelem state))))
	   (y (shuffle (loop for j from 1 to size collect
			     (random maxelem state)))))
       (let ((z (nset-exclusive-or-with-check x y)))
	 (let ((is-good (check-set-exclusive-or x y z)))
	   (unless is-good (return (values x y z)))))))
    nil))
|#

(defun subsetp-with-check (x y &key (key 'no-key) test test-not)
  (let ((xcopy (make-scaffold-copy x))
	(ycopy (make-scaffold-copy y)))
    (let ((result
	   (apply #'subsetp x y
		  `(,@(unless (eqt key 'no-key)
			`(:key ,key))
		      ,@(when test `(:test ,test))
		      ,@(when test-not `(:test-not ,test-not))))))
      (cond
       ((and (check-scaffold-copy x xcopy)
	     (check-scaffold-copy y ycopy))
	(not (not result)))
       (t 'failed)))))

(defun safe-elt (x n)
  (classify-error* (elt x n)))

(defmacro defstruct* (&body args)
  `(eval-when (load eval compile)
     (handler-case (eval '(defstruct ,@args))
		   (serious-condition () nil))))


(defun sort-package-list (x)
  (sort (copy-list x)
	#'string<
	:key #'package-name))

(defun sort-symbols (sl)
  (sort (copy-list sl)
	#'(lambda (x y)
	    (or
	     (string< (symbol-name x)
		      (symbol-name y))
	     (and (string= (symbol-name x)
			   (symbol-name y))
		  (string< (package-name (symbol-package x))
			   (package-name (symbol-package y))))))))

(defun num-symbols-in-package (p)
  (let ((num 0))
    (declare (fixnum num))
    (do-symbols (s p num)
      (incf num))))

(defun num-external-symbols-in-package (p)
  (let ((num 0))
    (declare (fixnum num))
    (do-external-symbols (s p num)
      (incf num))))

(defun safely-delete-package (package-designator)
  (let ((package (find-package package-designator)))
    (when package
      (let ((used-by (package-used-by-list package)))
	(dolist (using-package used-by)
	  (unuse-package package using-package)))
      (delete-package package))))

(defconstant +fail-count-limit+ 20)

(defmacro test-with-package-iterator (package-list-expr &rest symbol-types)
  "Build an expression that tests the with-package-iterator form."
  (let ((name (gensym))
	(cht-var (gensym))
	(pkg-list-var (gensym)))
    `(let ((,cht-var (make-hash-table))
	   (,pkg-list-var ,package-list-expr)
	   (fail-count 0))
	 (with-package-iterator (,name ,pkg-list-var
				       ,@(copy-list symbol-types))
	   ;; For each symbol, check that name is returning appropriate
	   ;; things
	   (loop
	     (block fail
	       (multiple-value-bind (more sym access pkg)
		   (,name)
		 (unless more (return nil))
		 (setf (gethash sym ,cht-var) t)  ;; note presence of symbol
		 ;; Check that its access status is in the list,
		 ;;  that pkg is a package,
		 ;;  that the symbol is in the package,
		 ;;  and that (in the package) it has the correct access type
		 (unless (member access (quote ,(copy-list symbol-types)))
		   (unless (> fail-count +fail-count-limit+)
		     (format t "Bad access type: ~S ==> ~A~%" sym access))
		   (when (= fail-count +fail-count-limit+)
		     (format t "Further messages suppressed~%"))
		   (incf fail-count)
		   (return-from fail nil))
		 
		 (unless (packagep pkg)
		   (unless (> fail-count +fail-count-limit+)
		     (format t "Not a package: ~S ==> ~S~%" sym pkg))
		   (when (= fail-count +fail-count-limit+)
		     (format t "Further messages suppressed~%"))
		   (incf fail-count)
		   (return-from fail nil))
		 (multiple-value-bind (sym2 access2)
		     (find-symbol (symbol-name sym) pkg)
		   (unless (or (eqt sym sym2)
			       (member sym2 (package-shadowing-symbols pkg)))
		     (unless (> fail-count +fail-count-limit+)
		       (format t "Not same symbol: ~S ~S~%" sym sym2))
		     (when (= fail-count +fail-count-limit+)
		       (format t "Further messages suppressed~%"))
		     (incf fail-count)
		     (return-from fail nil))
		   (unless  (eqt access access2)
		     (unless (> fail-count +fail-count-limit+)
		       (format t "Not same access type: ~S ~S ~S~%"
			       sym access access2))
		     (when (= fail-count +fail-count-limit+)
		       (format t "Further messages suppressed~%"))
		     (incf fail-count)
		     (return-from fail nil)))))))
	 ;; now, check that each symbol in each package has
	 ;; been properly found
	 (loop
	     for p in ,pkg-list-var do
	       (block fail
		 (do-symbols (sym p)
		   (multiple-value-bind (sym2 access)
		       (find-symbol (symbol-name sym) p)
		     (unless (eqt sym sym2)
		       (unless (> fail-count +fail-count-limit+)
			 (format t "Not same symbol (2): ~S ~S~%"
				 sym sym2))
		       (when (= fail-count +fail-count-limit+)
			 (format t "Further messages suppressed~%"))
		       (incf fail-count)
		       (return-from fail nil))
		     (unless (or (not (member access
					      (quote ,(copy-list symbol-types))))
				 (gethash sym ,cht-var))
		       (format t "Symbol not found: ~S~%" sym)
		       (incf fail-count)
		       (return-from fail nil))))))
	 (or (zerop fail-count) fail-count))))

(defun with-package-iterator-internal (packages)
  (test-with-package-iterator packages :internal))

(defun with-package-iterator-external (packages)
  (test-with-package-iterator packages :external))

(defun with-package-iterator-inherited (packages)
  (test-with-package-iterator packages :inherited))

(defun with-package-iterator-all (packages)
  (test-with-package-iterator packages :internal :external :inherited))

(defun frob-simple-condition (c expected-fmt &rest expected-args)
  "Try out the format control and format arguments of a simple-condition C,
   but make no assumptions about what they print as, only that they
   do print."
  (declare (ignore expected-fmt expected-args))
  (and (typep c 'simple-condition)
       (let ((fc (simple-condition-format-control c))
	     (args (simple-condition-format-arguments c)))
	 (and
	  (stringp (apply #'format nil fc args))
	  t))))

(defun frob-simple-error (c expected-fmt &rest expected-args)
  (and (typep c 'simple-error)
       (apply #'frob-simple-condition c expected-fmt expected-args)))

(defun frob-simple-warning (c expected-fmt &rest expected-args)
  (and (typep c 'simple-warning)
       (apply #'frob-simple-condition c expected-fmt expected-args)))

(defparameter *array-element-types*
  '(t (integer 0 0)
      bit (unsigned-byte 8) (unsigned-byte 16)
      (unsigned-byte 32) float short-float
      single-float double-float long-float
      nil character base-char symbol boolean null))

(defun collect-properties (plist prop)
  "Collect all the properties in plist for a property prop."
  (loop for e on plist by #'cddr
	when (eql (car e) prop)
	collect (cadr e)))
