;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Apr  9 19:32:56 1998
;;;; Contains: A global variable containing a list of
;;;;           as many kinds of CL objects as we can think of
;;;;           This list is used to test many other CL functions

(in-package :cl-test)

(defvar *condition-types*
    '(arithmetic-error
      cell-error
      condition
      control-error
      division-by-zero
      end-of-file
      error
      file-error
      floating-point-inexact
      floating-point-invalid-operation
      floating-point-underflow
      floating-point-overflow
      package-error
      parse-error
      print-not-readable
      program-error
      reader-error
      serious-condition
      simple-condition
      simple-error
      simple-type-error
      simple-warning
      storage-condition
      stream-error
      style-warning
      type-error
      unbound-slot
      unbound-variable
      undefined-function
      warning))

(defvar *condition-objects*
  (loop for tp in *condition-types* append
	(handler-case (list (make-condition tp))
		      (error () nil))))

(defvar *standard-package-names*
  '("COMMON-LISP" "COMMON-LISP-USER" "KEYWORD"))

(defvar *package-objects*
  (loop for pname in *standard-package-names* append
	(handler-case (let ((pkg (find-package pname)))
			(and pkg (list pkg)))
		      (error () nil))))

(defvar *integers*
    (remove-duplicates
     `(
       0
       ;; Integers near the fixnum/bignum boundaries
       ,@(loop for i from -5 to 5 collect (+ i most-positive-fixnum))
       ,@(loop for i from -5 to 5 collect (+ i most-negative-fixnum))
       ;; Powers of two, negatives, and off by one.
       ,@(loop for i from 1 to 64 collect (ash 1 i))
       ,@(loop for i from 1 to 64 collect (1- (ash 1 i)))
       ,@(loop for i from 1 to 64 collect (ash -1 i))
       ,@(loop for i from 1 to 64 collect (1+ (ash -1 i)))
       ;; A big integer
       ,(expt 17 50)
       ;; Some arbitrarily chosen integers
       12387131 1272314 231 -131 -561823 23713 -1234611312123 444121 991)))

(defvar *floats*
    (append
     (loop for sym in '(pi
			most-positive-short-float
			least-positive-short-float
			least-positive-normalized-short-float
			most-positive-double-float
			least-positive-double-float
			least-positive-normalized-double-float
			most-positive-long-float
			least-positive-long-float
			least-positive-normalized-long-float
			most-positive-single-float
			least-positive-single-float
			least-positive-normalized-single-float
			most-negative-short-float
			least-negative-short-float
			least-negative-normalized-short-float
			most-negative-single-float
			least-negative-single-float
			least-negative-normalized-single-float
			most-negative-double-float
			least-negative-double-float
			least-negative-normalized-double-float
			most-negative-long-float
			least-negative-long-float
			least-negative-normalized-long-float
			short-float-epsilon
			short-float-negative-epsilon
			single-float-epsilon
			single-float-negative-epsilon
			double-float-epsilon
			double-float-negative-epsilon
			long-float-epsilon
			long-float-negative-epsilon)
	   when (boundp sym) collect (symbol-value sym))
     (list
      0.0 1.0 -1.0 313123.13 283143.231 -314781.9
      1.31283d2 834.13812D-45
      8131238.1E14 -4618926.231e-2
      -37818.131F3 81.318231f-19
      1.31273s3 12361.12S-7
      6124.124l0 13123.1L-23)))

(defvar *ratios*
    '(1/3 1/1000 1/1000000000000000 -10/3 -1000/7 -987129387912381/13612986912361
      189729874978126783786123/1234678123487612347896123467851234671234))

(defvar *complexes*
    '(#C(0.0 0.0)
      #C(1.0 0.0)
      #C(0.0 1.0)
      #C(1.0 1.0)
      #C(-1.0 -1.0)
      #C(1289713.12312 -9.12681271)
      #C(1.0D100 1.0D100)
      #C(-1.0D-100 -1.0D-100)))

(defvar *numbers*
    (append *integers*
	    *floats*
	    *ratios*
	    *complexes*))

(defun try-to-read-chars (&rest namelist)
  (loop
    for name in namelist append
	(handler-case
	    (list (read-from-string
		   (concatenate 'string "\#\\" name)))
	  (error () nil))))

(defvar *characters*
    (remove-duplicates
     `(#\Newline
       #\Space
       ,@(try-to-read-chars "Rubout"
			    "Page"
			    "Tab"
			    "Backspace"
			    "Return"
			    "Linefeed"
			    "Null")
       #\a #\A #\0 #\9 #\. #\( #\) #\[ #\]
       )))


(defvar *strings*
    (append
     (and (code-char 0)
	  (list
	   (make-string 1 :initial-element (code-char 0))
	   (make-string 10 :initial-element (code-char 0))))
     (list
      "" "A" "a" "0" "abcdef"
      "~!@#$%^&*()_+`1234567890-=<,>.?/:;\"'{[}]|\\ abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWYXZ"
      (make-string 100000 :initial-element #\g)
      (let ((s (make-string 256)))
	(loop
	    for i from 0 to 255
	    do (let ((c (code-char i)))
		 (when c
		   (setf (elt s i) c))))
	s)
      )))

(defvar *conses*
    (list
     (list 'a 'b)
     (list nil)
     (list 1 2 3 4 5 6)))

(defvar *circular-conses*
    (list
     (let ((s (copy-list '(a b c d))))
       (nconc s s)
       s)
     (let ((s (list nil)))
       (setf (car s) s)
       s)
     (let ((s (list nil)))
       (setf (car s) s)
       (setf (cdr s) s))))

(defvar *booleans* '(nil t))
(defvar *keywords* '(:a :b :|| :|a| :|1234|))
(defvar *uninterned-symbols*
  (list '#:nil '#:t '#:foo '#:||))
(defvar *cl-test-symbols*
    `(,(intern "a" :cl-test)
      ,(intern "" :cl-test)
      ,@(and (code-char 0)
	     (list (intern (make-string 1 :initial-element (code-char 0)) :cl-test)))
      ,@(and (code-char 0)
	     (let* ((s (make-string 10 :initial-element (code-char 0)))
		    (s2 (copy-seq s))
		    (s3 (copy-seq s)))
	       (setf (subseq s 3 4) "a")
	       (setf (subseq s2 4 5) "a")
	       (setf (subseq s3 4 5) "a")
	       (setf (subseq s3 7 8) "b")
	       (list (intern s :cl-test)
		     (intern s2 :cl-test)
		     (intern s3 :cl-test))))
      ))

(defvar *cl-user-symbols*
    '(cl-user::foo cl-user::x
      cl-user::cons cl-user::lambda
      cl-user::*print-readably* cl-user::push))
	  
(defvar *symbols*
    (append *booleans* *keywords* *uninterned-symbols*
	    *cl-test-symbols*
	    *cl-user-symbols*))

(defvar *array-dimensions*
    (loop
	for i from 0 to 8 collect
	  (loop for j from 1 to i collect 2)))

(defvar *default-array-target* (make-array '(300)))

(defvar *arrays*
    (append
     (list (make-array '10))
     (mapcar #'make-array *array-dimensions*)
     
     ;; typed arrays
     (loop for tp in '(fixnum float bit character base-char
		       (signed-byte 8) (unsigned-byte 8))
	 append
	   (loop
	       for d in *array-dimensions*
	       collect (make-array d :element-type tp)))

     ;; adjustable arrays
     (loop
       for d in *array-dimensions*
	 collect (make-array d :adjustable t))

     ;; Displaced arrays
     (loop
      for d in *array-dimensions*
      for i from 1
      collect (make-array d :displaced-to *default-array-target*
			  :displaced-index-offset i))

     (list
      #()
      #*
      #*00000
      #*1010101010101101)

     ;; Integer arrays
     (list
      (make-array '(10) :element-type '(integer 0 (256))
		  :initial-contents '(8 9 10 11 12 1 2 3 4 5))
      (make-array '(10) :element-type '(integer -128 (128))
		  :initial-contents '(8 9 -10 11 -12 1 -2 -3 4 5))
      (make-array '(6) :element-type '(integer 0 (#.(ash 1 16)))
		  :initial-contents '(5 9 100 1312 23432 87))
      (make-array '(4) :element-type '(integer 0 (#.(ash 1 28)))
		  :initial-contents '(100000 231213 8123712 19))
      (make-array '(4) :element-type '(integer 0 (#.(ash 1 32)))
		  :initial-contents '(#.(1- (ash 1 32)) 0 872312 10000000))
      
      (make-array nil :element-type '(integer 0 (256))
		  :initial-element 14)
      (make-array '(2 2) :element-type '(integer 0 (256))
		  :initial-contents '((34 98)(14 119)))
      )

     ;; Float arrays
     (list
      (make-array '(5) :element-type 'short-float
		  :initial-contents '(1.0s0 2.0s0 3.0s0 4.0s0 5.0s0))
      (make-array '(5) :element-type 'single-float
		  :initial-contents '(1.0f0 2.0f0 3.0f0 4.0f0 5.0f0))
      (make-array '(5) :element-type 'double-float
		  :initial-contents '(1.0d0 2.0d0 3.0d0 4.0d0 5.0d0))
      (make-array '(5) :element-type 'long-float
		  :initial-contents '(1.0l0 2.0l0 3.0l0 4.0l0 5.0l0))
      )
     
     ;; more kinds of arrays here later
     ))

(defvar *hash-tables*
    (list
     (make-hash-table)
     (make-hash-table :test #'eq)
     (make-hash-table :test #'eql)
     (make-hash-table :test #'equal)
     #-(or GCL CMU ECL) (make-hash-table :test #'equalp)
     ))

(defvar *pathnames*
    (list
     (make-pathname :name "foo")
     (make-pathname :name "bar")
     (make-pathname :name "foo" :type "txt")
     (make-pathname :name "bar" :type "txt")
     (make-pathname :name :wild)
     (make-pathname :name :wild :type "txt")
     ))

(defvar *streams*
    (remove-duplicates
     (remove-if
      #'null
      (list
       *debug-io*
       *error-output*
       *query-io*
       *standard-input*
       *standard-output*
       *terminal-io*
       *trace-output*))))

(defvar *readtables*
    (list *readtable*
	  (copy-readtable)))

(defstruct foo-structure
  x y z)

(defstruct bar-structure
  x y z)

(defvar *structures*
    (list
     (make-foo-structure :x 1 :y 'a :z nil)
     (make-foo-structure :x 1 :y 'a :z nil)
     (make-bar-structure :x 1 :y 'a :z nil)
     ))

(defvar *functions*
  (list #'cons #'car #'append #'values
	(macro-function 'cond)
	#'(lambda (x) x)))

(defvar *random-states*
  (list (make-random-state)))

(defvar *universe*
    (remove-duplicates
     (append
      *symbols*
      *numbers*
      *characters*
      (mapcar #'copy-seq *strings*)
      *conses*
      *condition-objects*
      *package-objects*
      *arrays*
      *hash-tables*
      *pathnames*
      *streams*
      *readtables*
      *structures*
      *functions*
      *random-states*
      nil)))

(defvar *mini-universe*
  (remove-duplicates
   (mapcar #'first
	   (list *symbols*
		 *numbers*
		 *characters*
		 (mapcar #'copy-seq *strings*)
		 *conses*
		 *condition-objects*
		 *package-objects*
		 *arrays*
		 *hash-tables*
		 *pathnames*
		 *streams*
		 *readtables*
		 *structures*
		 *functions*
		 *random-states*))))


