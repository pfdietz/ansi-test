;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 28 17:10:18 1998
;;;; Contains: Aux. functions for CL-TEST

(in-package :cl-test)
(use-package :rt)

(declaim (optimize (safety 3)))

(defun make-int-list (n)
  (loop for i from 0 to (1- n) collect i))

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

(defmacro classify-error (form)
"Evaluate form in safe mode, returning its value if there is no error.
If an error does occur, return a symbol classify the error, or allow
the condition to go uncaught if it cannot be classified."
`(locally (declare (optimize (safety 3)))
  (handler-case ,form
     (undefined-function () 'undefined-function)
     (program-error () 'program-error)
     (type-error    () 'type-error))))

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

(defun is-eq-p (x) #'(lambda (y) (eq x y)))
(defun is-not-eq-p (x) #'(lambda (y) (not (eq x y))))

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

(defconstant +standard-chars+
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789~!@#$%^&*()_+|\\=-`{}[]:\";'<>?,./ 
")

(defconstant +alpha-chars+ (subseq +standard-chars+ 0 52))
(defconstant +lower-case-chars+ (subseq +alpha-chars+ 0 26))
(defconstant +upper-case-chars+ (subseq +alpha-chars+ 26 52))
(defconstant +alphanumeric-chars+ (subseq +standard-chars+ 0 62))
(defconstant +digit-chars+ "0123456789")
(defconstant +extended-digit-chars+ "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(defconstant +code-chars+
  (coerce (loop for i from 0 below 256
		for c = (code-char i)
		when c collect c)
	  'string))
(defconstant +rev-code-chars+ (reverse +code-chars+))
