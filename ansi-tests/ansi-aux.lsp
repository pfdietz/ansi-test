;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 28 17:10:18 1998
;;;; Contains: Aux. functions for CL-TEST

(in-package :cl-test)
(use-package :rt)

(declaim (optimize (safety 3)))

;;; Comparison functions that are like various builtins,
;;; but are guaranteed to return T for true.

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


;;; A function for coercing truth values to BOOLEAN

(defun notnot (x) (not (not x)))

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

;;; *universe* is defined elsewhere -- it is a list of various
;;; lisp objects used when stimulating things in various tests.
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
;;; The function SUBTYPEP returns two generalized booleans.
;;; This auxiliary function returns two booleans instead
;;; (which makes it easier to write tests).
;;;
(defun subtypep* (obj type)
  (multiple-value-bind (result good)
      (subtypep obj type)
    (values (notnot result)
	    (notnot good))))

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

(defvar *disjoint-types-list*
    '(cons symbol array
      number character hash-table function readtable package
      pathname stream random-state condition restart))

(defun is-t-or-nil (e)
  (or (eqt e t) (eqt e nil)))

(declaim (special *subtype-table*))

(defun types-6-body ()
  (loop
      for p in *subtype-table* count
      (let ((tp (car p)))
	(when (and (not (member tp '(sequence cons list t)))
		   (not (subtypep* tp 'atom)))
	  (format t "~%Problem!  Did not find to be an atomic type: ~S" tp)
	  t))))

(defvar *type-list* nil)
(defvar *supertype-table* nil)
(declaim (special *subtype-table*))

(defun types-9-body ()
  (let ((tp-list (append '(keyword atom list)
			 (loop for p in *subtype-table* collect (car p))))
	(result-list))
    (setf tp-list (remove-duplicates tp-list))
    ;; TP-LIST is now a list of unique CL type names
    ;; Store it in *TYPE-LIST* so we can inspect it later if this test
    ;; fails.  The variable is also used in test TYPES-9A
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
      ;; and use in test TYPES-9A
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

;;; TYPES-9-BODY returns a list of triples (T1 T2 T3)
;;; where (AND (SUBTYPEP T1 T2) (SUBTYPEP T2 T3) (NOT (SUBTYPEP T1 T3)))
;;;  (and where SUBTYPEP succeeds in each case, returning true as its
;;;   second return value.)

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

(defun nset-exclusive-or-with-check (x y &key (key 'no-key)
				       test test-not)
  (setf x (copy-list x))
  (setf y (copy-list y))
  (apply #'nset-exclusive-or
	 x y
	 `(,@(unless (eqt key 'no-key) `(:key ,key))
	     ,@(when test `(:test ,test))
	     ,@(when test-not `(:test-not ,test-not)))))

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
