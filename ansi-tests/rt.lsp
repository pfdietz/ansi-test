;-*-syntax:COMMON-LISP;Package:(RT :use "COMMON-LISP" :colon-mode :external)-*-

#|----------------------------------------------------------------------------|
 | Copyright 1990 by the Massachusetts Institute of Technology, Cambridge MA. |
 |                                                                            |
 | Permission  to  use,  copy, modify, and distribute this software  and  its |
 | documentation for any purpose  and without fee is hereby granted, provided |
 | that this copyright  and  permission  notice  appear  in  all  copies  and |
 | supporting  documentation,  and  that  the  name  of M.I.T. not be used in |
 | advertising or  publicity  pertaining  to  distribution  of  the  software |
 | without   specific,   written   prior   permission.      M.I.T.  makes  no |
 | representations  about  the  suitability of this software for any purpose. |
 | It is provided "as is" without express or implied warranty.                |
 |                                                                            |
 |  M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,  INCLUDING  |
 |  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL  |
 |  M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAMAGES  OR  |
 |  ANY  DAMAGES  WHATSOEVER  RESULTING  FROM  LOSS OF USE, DATA OR PROFITS,  |
 |  WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER  TORTIOUS  ACTION,  |
 |  ARISING  OUT  OF  OR  IN  CONNECTION WITH THE USE OR PERFORMANCE OF THIS  |
 |  SOFTWARE.                                                                 |
 |----------------------------------------------------------------------------|#

;This is the December 19, 1990 version of the regression tester.

(in-package :regression-test)

(declaim (ftype (function (t) t) get-entry expanded-eval do-entries))
(declaim (type list *entries*))
(declaim (ftype (function (t &rest t) t) report-error))
(declaim (ftype (function (t &optional t) t) do-entry))

(defvar *test* nil "Current test name")
(defvar *do-tests-when-defined* nil)
(defvar *entries* '(nil) "Test database")
(defvar *in-test* nil "Used by TEST")
(defvar *debug* nil "For debugging")
(defvar *catch-errors* t "When true, causes errors in a test to be caught.")
(defvar *print-circle-on-failure* nil
  "Failure reports are printed with *PRINT-CIRCLE* bound to this value.")

(defvar *compile-tests* nil "When true, compile the tests before running them.")
(defvar *expanded-eval* nil "When true, convert the tests into a form that is less likely to have compiler optimizations.")
(defvar *optimization-settings* '((safety 3)))

(defvar *expected-failures* nil
  "A list of test names that are expected to fail.")

(defvar *notes* (make-hash-table :test 'equal)
  "A mapping from names of notes to note objects.")
  
(defstruct (entry (:conc-name nil))
  pend name props form vals)

;;; Note objects are used to attach information to tests.
;;; A typical use is to mark tests that depend on a particular
;;; part of a set of requirements, or a particular interpretation
;;; of the requirements.

(defstruct note
  name  
  contents
  disabled ;; When true, tests with this note are considered inactive
  )

;; (defmacro vals (entry) `(cdddr ,entry))

(defmacro defn (entry)
  (let ((var (gensym)))
    `(let ((,var ,entry))
       (list* (name ,var) (form ,var) (vals ,var)))))

(defun pending-tests ()
  (do ((l (cdr *entries*) (cdr l))
       (r nil))
      ((null l) (nreverse r))
    (when (pend (car l))
      (push (name (car l)) r))))

(defun rem-all-tests ()
  (setq *entries* (list nil))
  nil)

(defun rem-test (&optional (name *test*))
  (do ((l *entries* (cdr l)))
      ((null (cdr l)) nil)
    (when (equal (name (cadr l)) name)
      (setf (cdr l) (cddr l))
      (return name))))

(defun get-test (&optional (name *test*))
  (defn (get-entry name)))

(defun get-entry (name)
  (let ((entry (find name (the list (cdr *entries*))
		     :key #'name
		     :test #'equal)))
    (when (null entry)
      (report-error t
        "~%No test with name ~:@(~S~)."
	name))
    entry))

(defun entry-notes (entry)
  (let* ((props (props entry))
	 (notes (getf props :notes)))
    (if (listp notes)
	notes
      (list notes))))

(defun has-disabled-note (entry)
  (let ((notes (entry-notes entry)))
    (loop for n in notes
	  for note = (if (note-p n) n
		       (gethash n *notes*))
	  thereis (and note (note-disabled note)))))

(defmacro deftest (name &rest body)
  (let* ((p body)
	 (properties
	  (loop while (keywordp (first p))
		unless (cadr p)
		do (error "Poorly formed deftest: ~A~%"
			  (list* 'deftest name body))
		append (list (pop p) (pop p))))
	 (form (pop p))
	 (vals p))
    `(add-entry (make-entry :pend t
			    :name ',name
			    :props ',properties
			    :form ',form
			    :vals ',vals))))

(defun add-entry (entry)
  (setq entry (copy-entry entry))
  (do ((l *entries* (cdr l))) (nil)
    (when (null (cdr l))
      (setf (cdr l) (list entry))
      (return nil))
    (when (equal (name (cadr l)) 
		 (name entry))
      (setf (cadr l) entry)
      (report-error nil
        "Redefining test ~:@(~S~)"
        (name entry))
      (return nil)))
  (when *do-tests-when-defined*
    (do-entry entry))
  (setq *test* (name entry)))

(defun report-error (error? &rest args)
  (cond (*debug* 
	 (apply #'format t args)
	 (if error? (throw '*debug* nil)))
	(error? (apply #'error args))
	(t (apply #'warn args)))
  nil)

(defun do-test (&optional (name *test*))
  (do-entry (get-entry name)))

(defun equalp-with-case (x y)
  "Like EQUALP, but doesn't do case conversion of characters.
   Currently doesn't work on arrays of dimension > 2."
  (cond
   ((eq x y) t)
   ((consp x)
    (and (consp y)
	 (equalp-with-case (car x) (car y))
	 (equalp-with-case (cdr x) (cdr y))))
   ((and (typep x 'array)
	 (= (array-rank x) 0))
    (equalp-with-case (aref x) (aref y)))
   ((typep x 'vector)
    (and (typep y 'vector)
	 (let ((x-len (length x))
	       (y-len (length y)))
	   (and (eql x-len y-len)
		(loop
		 for e1 across x
		 for e2 across y
		 always (equalp-with-case e1 e2))))))
   ((and (typep x 'array)
	 (typep y 'array)
	 (not (equal (array-dimensions x)
		     (array-dimensions y))))
    nil)

   ((typep x 'array)
    (and (typep y 'array)
	 (let ((size (array-total-size x)))
	   (loop for i from 0 below size
		 always (equalp-with-case (row-major-aref x i)
					  (row-major-aref y i))))))

   (t (eql x y))))

(defun do-entry (entry &optional
		       (s *standard-output*))
  (catch '*in-test*
    (setq *test* (name entry))
    (setf (pend entry) t)
    (let* ((*in-test* t)
	   ;; (*break-on-warnings* t)
	   (aborted nil)
	   r)
      ;; (declare (special *break-on-warnings*))

      (block aborted
	(setf r
	      (flet ((%do
		      ()
		      (cond
		       (*compile-tests*
			(multiple-value-list
			 (funcall (compile
				   nil
				   `(lambda ()
				      (declare
				       (optimize ,@*optimization-settings*))
				      ,(form entry))))))
		       (*expanded-eval*
			(multiple-value-list
			 (expanded-eval (form entry))))
		       (t
			(multiple-value-list
			 (eval (form entry)))))))
		(if *catch-errors*
		    (handler-bind
		     (#-ecl (style-warning #'muffle-warning)
			    (error #'(lambda (c)
				       (setf aborted t)
				       (setf r (list c))
				       (return-from aborted nil))))
		     (%do))
		  (%do)))))

      (setf (pend entry)
	    (or aborted
		(not (equalp-with-case r (vals entry)))))
      
      (when (pend entry)
	(let ((*print-circle* *print-circle-on-failure*))
	  (format s "~&Test ~:@(~S~) failed~
                   ~%Form: ~S~
                   ~%Expected value~P: ~
                      ~{~S~^~%~17t~}~%"
		  *test* (form entry)
		  (length (vals entry))
		  (vals entry))
	  (format s "Actual value~P: ~
                      ~{~S~^~%~15t~}.~%"
		  (length r) r)))))
  (when (not (pend entry)) *test*))

(defun expanded-eval (form)
  "Split off top level of a form and eval separately.  This reduces the chance that
   compiler optimizations will fold away runtime computation."
  (if (not (consp form))
      (eval form)
   (let ((op (car form)))
     (cond
      ((eq op 'let)
       (let* ((bindings (loop for b in (cadr form)
			      collect (if (consp b) b (list b nil))))
	      (vars (mapcar #'car bindings))
	      (binding-forms (mapcar #'cadr bindings)))
	 (apply
	  (the function
	    (eval `(lambda ,vars ,@(cddr form))))
	  (mapcar #'eval binding-forms))))
      ((and (eq op 'let*) (cadr form))
       (let* ((bindings (loop for b in (cadr form)
			      collect (if (consp b) b (list b nil))))
	      (vars (mapcar #'car bindings))
	      (binding-forms (mapcar #'cadr bindings)))
	 (funcall
	  (the function
	    (eval `(lambda (,(car vars) &aux ,@(cdr bindings)) ,@(cddr form))))
	  (eval (car binding-forms)))))
      ((eq op 'progn)
       (loop for e on (cdr form)
	     do (if (null (cdr e)) (return (eval (car e)))
		  (eval (car e)))))
      ((and (symbolp op) (fboundp op)
	    (not (macro-function op))
	    (not (special-operator-p op)))
       (apply (symbol-function op)
	      (mapcar #'eval (cdr form))))
      (t (eval form))))))

(defun continue-testing ()
  (if *in-test*
      (throw '*in-test* nil)
      (do-entries *standard-output*)))

(defun do-tests (&optional
		 (out *standard-output*))
  (dolist (entry (cdr *entries*))
    (setf (pend entry) t))
  (if (streamp out)
      (do-entries out)
      (with-open-file 
	  (stream out :direction :output)
	(do-entries stream))))

(defun do-entries (s)
  (format s "~&Doing ~A pending test~:P ~
             of ~A tests total.~%"
          (count t (the list (cdr *entries*)) :key #'pend)
	  (length (cdr *entries*)))
  (dolist (entry (cdr *entries*))
    (when (and (pend entry)
	       (not (has-disabled-note entry)))
      (format s "~@[~<~%~:; ~:@(~S~)~>~]"
	      (do-entry entry s))))
  (let ((pending (pending-tests))
	(expected-table (make-hash-table :test #'equal)))
    (dolist (ex *expected-failures*)
      (setf (gethash ex expected-table) t))
    (let ((new-failures
	   (loop for pend in pending
		 unless (gethash pend expected-table)
		 collect pend)))
      (if (null pending)
	  (format s "~&No tests failed.")
	(progn
	  (format s "~&~A out of ~A ~
                   total tests failed: ~
                   ~:@(~{~<~%   ~1:;~S~>~
                         ~^, ~}~)."
		  (length pending)
		  (length (cdr *entries*))
		  pending)
	  (if (null new-failures)
	      (format s "~&No unexpected failures.")
	    (when *expected-failures*
	      (format s "~&~A unexpected failures: ~
                   ~:@(~{~<~%   ~1:;~S~>~
                         ~^, ~}~)."
		    (length new-failures)
		    new-failures)))
	  ))
      (null pending))))

;;; Note handling functions and macros

(defmacro defnote (name contents)
  `(eval-when #+gcl (load eval) #-gcl (:load-toplevel :execute)
     (let ((note (make-note :name ,name :contents ,contents)))
       (setf (gethash *notes* (note-name note)) note)
       note)))

