;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Oct  6 05:04:45 2003
;;;; Contains: Generating random types and testing relationships on them

(in-package :cl-test)

(compile-and-load "random-aux.lsp")
(compile-and-load "random-int-form.lsp")

(defun make-random-type (size)
  (if (<= size 1)
      (rcase
       (1 nil)
       (1 t)
       (1 `(eql ,(let ((r (ash 1 (random 45))))
		   (random-from-interval r (- r)))))
       (1 (random-from-seq #(integer unsigned-byte ratio rational real float
			     short-float single-float double-float
			     long-float complex symbol cons)))
       (1
	(let* ((len (random *maximum-random-int-bits*))
	       (r1 (ash 1 len))
	       (r2 (+ r1 r1))
	       (x (- (random r2) r1))
	       (y (- (random r2) r1))
	       (lo (min x y))
	       (hi (max x y)))
	  `(integer ,lo ,hi)))
       )
    (rcase
     (2 (destructuring-bind (s1 s2)
	    (random-partition (1- size) 2)
	  (let ((op (random-from-seq #(cons cons and or))))
	    `(,op ,(make-random-type s1)
		  ,(make-random-type s2)))))
     (1 `(not ,(make-random-type (1- size))))
     )))

(defun test-random-types (n size)
  (loop for t1 = (make-random-type size)
	for t2 = (make-random-type size)
	for i from 0 below n
	;; do (print (list t1 t2))
	do (when (and (= (mod i 100) 0) (> i 0))
	     (format t "~A " i) (finish-output *standard-output*))
	when (test-types t1 t2)
	collect (list t1 t2)
	finally (terpri)))
  

(defun test-types (t1 t2)
  (multiple-value-bind (sub success)
      (subtypep t1 t2)
    (when success
      (if sub
	  (check-all-subtypep t1 t2)
	(let ((nt1 `(not ,t1))
	      (nt2 `(not ,t2)))
	  (subtypep nt2 nt1))))))

(defun prune-type (tp try-fn)
  (declare (type function try-fn))
  (flet ((try (x) (funcall try-fn x)))
    (cond
     ((member tp '(nil t)))
     ((symbolp tp)
      (try nil)
      (try t))
     ((consp tp)
      (try nil)
      (try t)
      (let ((op (first tp))
	    (args (rest tp)))
	(case op
	  ((cons)
	   (try 'cons)
	   (prune-list args
		       #'prune-type
		       #'(lambda (args) (try `(cons ,@args)))))
	  ((integer real float ratio single-float double-float short-float long-float)
	   (try op))
	  ((or and)
	   (mapc try-fn args)
	   (prune-list args
		       #'prune-type
		       #'(lambda (args) (try (cons op args)))))
	  ((not)
	   (let ((arg (first args)))
	     (try arg)
	     (when (and (consp arg)
			(eq (car arg) 'not))
	       (try (second arg)))
	     (prune-type arg #'(lambda (arg) (try `(not ,arg))))))
	  ((member)
	   (dolist (arg (cdr tp))
	     (try `(eql ,arg)))
	   (when (cddr tp)
	   (try `(member ,@(cddr tp)))))
	  )))))
  (values))

(defun prune-type-pair (t1 t2)
  (let (changed)
    (loop
     do (flet ((%try2 (new-tp)
		      (when (test-types t1 new-tp)
			(print "Success in first loop")
			(print new-tp)
			(setq t2 new-tp
			      changed t)
			(throw 'success nil))))
	  (catch 'success
	    (prune-type t2 #'%try2)))
     do (flet ((%try1 (new-tp)
		      (when (test-types new-tp t2)
			(print "Success in second loop")
			(print new-tp)
			(setq t1 new-tp
			      changed t)
			(throw 'success nil))))
	  (catch 'success
	    (prune-type t1 #'%try1)))
     while changed
     do (setq changed nil)))
  (values t1 t2))


     
  
			   
	 


  