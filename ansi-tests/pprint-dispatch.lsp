;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jun 12 13:14:53 2004
;;;; Contains: Tests of PPRINT-DISPATCH, SET-PPRINT-DISPATCH

(in-package :cl-test)

(deftest pprint-dispatch.1
  (loop for x in *universe*
	for vals = (multiple-value-list (pprint-dispatch x))
	for vals2 = (multiple-value-list (pprint-dispatch
					  x
					  *print-pprint-dispatch*))
	unless
	(and (= (length vals) 2)
	     (= (length vals2) 2)
	     (destructuring-bind (fun foundp)
		 vals
	       (if foundp
		   (and (or (typep fun 'function)
			    (and (symbolp fun)
				 (symbol-function fun)))
			(destructuring-bind (fun2 foundp2)
			    vals2
			  (and (equal fun fun2)
			       foundp2)))
		 (not (cadr vals2)))))
	collect (list x vals vals2))
  nil)

(deftest pprint-dispatch.2
  (loop for sym in *cl-symbols*
	for x = (list sym nil nil)
	for vals = (multiple-value-list (pprint-dispatch x))
	for vals2 = (multiple-value-list (pprint-dispatch
					  x
					  *print-pprint-dispatch*))
	unless
	(and (= (length vals) 2)
	     (= (length vals2) 2)
	     (destructuring-bind (fun foundp)
		 vals
	       (if foundp
		   (and (or (typep fun 'function)
			    (and (symbolp fun)
				 (symbol-function fun)))
			(destructuring-bind (fun2 foundp2)
			    vals2
			  (and (equal fun fun2)
			       foundp2)))
		 (not (cadr vals2)))))
	collect (list x vals vals2))
  nil)

(deftest pprint-dispatch.3
  (my-with-standard-io-syntax
   (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil))
	 (*print-readably* nil)
	 (*print-escape* nil)
	 (*print-pretty* t))
     (let ((f #'(lambda (stream obj)
		  (declare (ignore obj))
		  (write "ABC" :stream stream))))
       (values
	(write-to-string '|X|)
	(set-pprint-dispatch '(eql |X|) f)
	(write-to-string '|X|)
	(set-pprint-dispatch '(eql |X|) nil)
	(write-to-string '|X|)))))
  "X" nil "ABC" nil "X")
       
(deftest pprint-dispatch.4
  (my-with-standard-io-syntax
   (loop for v1 in (delete-if-not #'realp *universe*)
	 unless
	 (equal
	  (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil))
		(*print-readably* nil)
		(*print-escape* nil)
		(*print-pretty* t))
	    (let ((f #'(lambda (stream obj)
			 (declare (ignore obj))
			 (write "ABC" :stream stream))))
	      (list
	       (write-to-string '|X|)
	       (set-pprint-dispatch '(eql |X|) f v1)
	       (write-to-string '|X|)
	       (set-pprint-dispatch '(eql |X|) nil)
	       (write-to-string '|X|))))
	  '("X" nil "ABC" nil "X"))
	 collect v1))
  nil)
  
(deftest pprint-dispatch.5
  (my-with-standard-io-syntax
   (loop for v1 in (delete-if-not #'realp *universe*)
	 unless
	 (equal
	  (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil))
		(*print-readably* nil)
		(*print-escape* nil)
		(*print-pretty* t))
	    (let ((f #'(lambda (stream obj)
			 (declare (ignore obj))
			 (write "ABC" :stream stream))))
	      (list
	       (write-to-string '|X|)
	       (set-pprint-dispatch '(eql |X|) f)
	       (write-to-string '|X|)
	      (set-pprint-dispatch '(eql |X|) nil v1)
	      (write-to-string '|X|))))
	  '("X" nil "ABC" nil "X"))
	 collect v1))
  nil)

(deftest pprint-dispatch.6
  (my-with-standard-io-syntax
   (let ((other-ppd-table (copy-pprint-dispatch nil))
	 (*print-pprint-dispatch* (copy-pprint-dispatch nil))
	 (*print-readably* nil)
	 (*print-escape* nil)
	 (*print-pretty* t))
     (let ((f #'(lambda (stream obj)
		  (declare (ignore obj))
		  (write "ABC" :stream stream))))
       (values
	(write-to-string '|X|)
	(set-pprint-dispatch '(eql |X|) f 0 other-ppd-table)
	(write-to-string '|X|)
	(let ((*print-pprint-dispatch* other-ppd-table))
	  (write-to-string '|X|))
	(set-pprint-dispatch '(eql |X|) f)
	(write-to-string '|X|)
	(set-pprint-dispatch '(eql |X|) nil)
	(write-to-string '|X|)))))
  "X" nil "X" "ABC" nil "ABC" nil "X")
