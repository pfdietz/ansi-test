;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 28 17:10:18 1998
;;;; Contains: Aux. functions for CL-TEST

(in-package :cl-test)
(use-package :rt)

(declaim (optimize (safety 3)))

(defun eqt (x y)
  "Like EQ, but guaranteed to return T for true."
  (if (eq x y) t nil))

(defun eqlt (x y)
  "Like EQL, but guaranteed to return T for true."
  (if (eql x y) t nil))

(defun equalt (x y)
  "Like EQUAL, but guaranteed to return T for true."
  (if (equal x y) t nil))

(defun equalpt (x y)
  "Like EQUALP, but guaranteed to return T for true."
  (if (equalp x y) t nil))

(defun =t (x &rest args)
  "Like =, but guaranteed to return T for true."
  (if (apply #'= x args) t nil))

(defun notnot (x) (not (not x)))

(defun make-int-list (n)
  (loop for i from 0 below n collect i))

(defun make-int-array (n &optional (fn #'make-array))
  (let ((a (funcall fn n)))
    (loop for i from 0 below n do (setf (aref a i) i))
    a))

(defun equal-array (a1 a2)
  (and (typep a1 'array)
       (typep a2 'array)
       (= (array-rank a1) (array-rank a2))
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
				      (row-major-aref a2 i))))))))))

(declaim (special *universe*))

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
  )))

(defmacro classify-error (form)
  `(classify-error* (eval ',form)))

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


;;;
;;; The function SUBTYPEP returns two generalized booleans.
;;; This auxiliary function returns two booleans instead
;;; (which makes it easier to write tests).
;;;
(defun subtypep* (obj type)
  (multiple-value-bind (result good)
      (subtypep obj type)
    (values (not (not result))
	    (not (not good)))))

;;; (eval-when (load eval compile)
;;;   (unless (fboundp 'complement)
;;;     (defun complement (fn)
;;;       #'(lambda (&rest args) (not (apply fn args))))))

(defun compose (&rest fns)
  (let ((rfns (reverse fns)))
    #'(lambda (x) (loop for f in rfns do (setf x (funcall f x))) x)))

(defun evendigitp (c)
  (not (not (find c "02468"))))

(defun odddigitp (c)
  (not (not (find c "13579"))))

(defun nextdigit (c)
  (cadr (member c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))))

(defun is-eq-p (x) #'(lambda (y) (eqt x y)))
(defun is-not-eq-p (x) #'(lambda (y) (not (eqt x y))))

(defun char-invertcase (c)
  (if (upper-case-p c) (char-downcase c)
    (char-upcase c)))

(defun string-invertcase (s)
  (map 'string #'char-invertcase s))


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

