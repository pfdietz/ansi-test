;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Jan 23 05:36:55 2004
;;;; Contains: Tests of OPEN

(in-package :cl-test)

;;; Input streams

(defun generator-for-element-type (type)
  (etypecase type
   ((member character base-char)
    #'(lambda (i) (aref "abcdefghijklmnopqrstuvwxyz" (mod i 26))))
   ((member signed-byte unsigned-byte bit)
    #'(lambda (i) (logand i 1)))
   (cons
    (let ((op (car type))
	  (arg1 (cadr type))
	  (arg2 (caddr type)))
      (ecase op
	(unsigned-byte
	 (let ((mask (1- (ash 1 arg1))))
	   #'(lambda (i) (logand i mask))))
	(signed-byte
	 (let ((mask (1- (ash 1 (1- arg1)))))
	   #'(lambda (i) (logand i mask))))
	(integer
	 (let* ((lo arg1)
		(hi arg2)
	       (lower-bound
		(etypecase lo
		  (integer lo)
		  (cons (1+ (car lo)))))
	       (upper-bound
		(etypecase hi
		  (integer hi)
		  (cons (1- (car hi)))))
	       (range (1+ (- upper-bound lower-bound))))
	   #'(lambda (i) (+ lower-bound (mod i range))))))))))

(compile 'generator-for-element-type)

(defmacro def-open-test (name args form expected
			      &key
			      (build-form nil build-form-p)
			      (element-type 'character element-type-p)
			      (pathname #p"tmp.dat"))
	  
  (when element-type-p
    (setf args (append args (list :element-type `',element-type))))

  (unless build-form-p
    (let ((write-element-form
	   (cond
	    ((subtypep element-type 'integer)
	     `(write-byte
	       (funcall (the function
			  (generator-for-element-type ',element-type)) i)
	       os))
	    ((subtypep element-type 'character)
	     `(write-char
	       (funcall (the function
			  (generator-for-element-type ',element-type)) i)
	       os)))))
      (setq build-form
	    `(with-open-file
	      (os pn :direction :output
		  ,@(if element-type-p
			`(:element-type ',element-type))
		  :if-exists :supersede)
	      (assert (open-stream-p os))
	      (dotimes (i 10) ,write-element-form)))))
			      
  `(deftest ,name
     (let ((pn ,pathname))
       (when (probe-file pn) (delete-file pn))
       ,build-form
       (let ((s (open pn ,@args)))
	 (unwind-protect
	     (progn
	       (assert (open-stream-p s))
	       (assert (typep s 'file-stream))
	       ,@(unless (member element-type '(signed-byte unsigned-byte))
		   `((assert (subtypep ',element-type
				       (stream-element-type s)))))
	       ,form)
	   (close s))))
     ,@expected))

(compile 'def-open-test)

(def-open-test open.1 () (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.2 (:direction :input)
  (values (read-line s nil)) ("abcdefghij") :element-type character)
(def-open-test open.3 (:direction :input)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.4 (:direction :input)
  (values (read-line s nil)) ("abcdefghij") :element-type base-char)
(def-open-test open.5 (:if-exists :error)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.6 (:if-exists :error :direction :input)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.7 (:if-exists :new-version)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.8 (:if-exists :new-version :direction :input)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.9 (:if-exists :rename)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.10 (:if-exists :rename :direction :input)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.11 (:if-exists :rename-and-delete)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.12 (:if-exists :rename-and-delete :direction :input)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.13 (:if-exists :overwrite)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.14 (:if-exists :overwrite :direction :input)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.15 (:if-exists :append)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.16 (:if-exists :append :direction :input)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.17 (:if-exists :supersede)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.18 (:if-exists :supersede :direction :input)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.19 (:if-exists nil)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.20 (:if-exists nil :direction :input)
  (values (read-line s nil)) ("abcdefghij"))

(def-open-test open.21 (:if-does-not-exist nil)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.22 (:if-does-not-exist nil :direction :input)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.23 (:if-does-not-exist :error)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.24 (:if-does-not-exist :error :direction :input)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.25 (:if-does-not-exist :create)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.26 (:if-does-not-exist :create :direction :input)
  (values (read-line s nil)) ("abcdefghij"))

(def-open-test open.27 (:external-format :default)
  (values (read-line s nil)) ("abcdefghij"))
(def-open-test open.28 (:external-format :default :direction :input)
  (values (read-line s nil)) ("abcdefghij"))

(def-open-test open.29 ()
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 0 1 0 1 0 1 0 1)) :element-type (unsigned-byte 1))
(def-open-test open.30 (:direction :input)
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 0 1 0 1 0 1 0 1)) :element-type (unsigned-byte 1))

(def-open-test open.31 ()
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 0 1 2 3 0 1)) :element-type (unsigned-byte 2))
(def-open-test open.32 (:direction :input)
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 0 1 2 3 0 1)) :element-type (unsigned-byte 2))

(def-open-test open.33 ()
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 0 1)) :element-type (unsigned-byte 3))
(def-open-test open.34 (:direction :input)
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 0 1)) :element-type (unsigned-byte 3))

(def-open-test open.35 ()
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 4))
(def-open-test open.36 (:direction :input)
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 4))

(def-open-test open.37 ()
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 5))
(def-open-test open.38 (:direction :input)
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 5))

(def-open-test open.39 ()
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 6))
(def-open-test open.40 (:direction :input)
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 6))

(def-open-test open.41 ()
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 7))
(def-open-test open.42 (:direction :input)
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 7))

(def-open-test open.43 ()
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 8))
(def-open-test open.44 (:direction :input)
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 8))

(def-open-test open.45 ()
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 9))
(def-open-test open.46 (:direction :input)
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 9))

(def-open-test open.47 ()
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 10))
(def-open-test open.48 (:direction :input)
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 10))

(def-open-test open.49 ()
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 20))
(def-open-test open.50 (:direction :input)
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 20))

(def-open-test open.51 ()
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 25))
(def-open-test open.52 (:direction :input)
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 25))

(def-open-test open.53 ()
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 30))
(def-open-test open.54 (:direction :input)
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 30))

(def-open-test open.55 ()
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 32))
(def-open-test open.56 (:direction :input)
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 32))

(def-open-test open.57 ()
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 33))
(def-open-test open.58 (:direction :input)
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 2 3 4 5 6 7 8 9)) :element-type (unsigned-byte 33))

(def-open-test open.59 ()
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 0 1 0 1 0 1 0 1)) :element-type unsigned-byte)
(def-open-test open.60 (:direction :input)
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 0 1 0 1 0 1 0 1)) :element-type unsigned-byte)

(def-open-test open.61 ()
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 0 1 0 1 0 1 0 1)) :element-type signed-byte)
(def-open-test open.62 (:direction :input)
  (let ((seq (make-array 10))) (read-sequence seq s) seq)
  (#(0 1 0 1 0 1 0 1 0 1)) :element-type signed-byte)


(def-open-test open.63 ()
  (values (read-line s nil)) ("abcdefghij")
  :pathname "tmp.dat")

(def-open-test open.64 ()
  (values (read-line s nil)) ("abcdefghij")
  :pathname (logical-pathname "CLTEST:TMP.DAT"))

;;; It works on recognizable subtypes.
(deftest open.65
  (let ((type '(or (integer 0 1) (integer 100 200)))
	(pn #p"tmp.dat")
	(vals '(0 1 100 120 130 190 200 1 0 150)))
    (or
     (not (subtypep type 'integer))
     (progn
       (with-open-file
	(os pn :direction :output
	    :element-type type
	    :if-exists :supersede)
	(dolist (e vals) (write-byte e os)))
       (let ((s (open pn :direction :input
		      :element-type type))
	     (seq (make-array 10)))
	 (unwind-protect
	     (progn (read-sequence seq s) seq)
	   (close s))
	 (notnot (every #'eql seq vals))))))
  t)

;;; Tests of file creation

(defmacro def-open-output-test
  (name args form expected
	&rest keyargs
	&key
	(element-type 'character)
	(build-form
	 `(dotimes (i 10)
	    ,(cond
	      ((subtypep element-type 'integer)
	       `(write-byte
		 (funcall (the function
			    (generator-for-element-type ',element-type)) i)
		 s))
	      ((subtypep element-type 'character)
	       `(write-char
		 (funcall (the function
			    (generator-for-element-type ',element-type)) i)
		 s)))))
	&allow-other-keys)
  `(def-open-test ,name (:direction :output ,@args)
     (progn
       ,build-form
       (assert (output-stream-p s))
       ,form)
     ,expected
     :build-form nil
     ,@keyargs))

(compile 'def-open-output-test)

(def-open-output-test open.output.1 ()
  (progn (close s) (with-open-file (is #p"tmp.dat")
				   (let ((seq (copy-seq "          ")))
				     (read-sequence seq is)
				     seq)))
  ("abcdefghij"))

(def-open-output-test open.output.2 ()
  (progn (close s) (with-open-file (is "tmp.dat")
				   (let ((seq (copy-seq "          ")))
				     (read-sequence seq is)
				     seq)))
  ("abcdefghij")
  :pathname "tmp.dat")

(def-open-output-test open.output.3
  ()
  (progn (close s) (with-open-file (is (logical-pathname "CLTEST:TMP.DAT"))
				   (let ((seq (copy-seq "          ")))
				     (read-sequence seq is)
				     seq)))
  ("abcdefghij")
  :pathname (logical-pathname "CLTEST:TMP.DAT"))

(def-open-output-test open.output.4 ()
  (progn (close s) (with-open-file (is #p"tmp.dat")
				   (let ((seq (copy-seq "          ")))
				     (read-sequence seq is)
				     seq)))
  ("abcdefghij")
  :element-type character)

(def-open-output-test open.output.5 ()
  (progn (close s) (with-open-file (is #p"tmp.dat"
				       :element-type 'base-char)
				   (let ((seq (copy-seq "          ")))
				     (read-sequence seq is)
				     seq)))
  ("abcdefghij")
  :element-type base-char)

(def-open-output-test open.output.6 ()
  (progn (close s) (with-open-file (is #p"tmp.dat"
				       :element-type '(integer 0 1))
				   (let ((seq (make-array 10)))
				     (read-sequence seq is)
				     seq)))
  (#(0 1 0 1 0 1 0 1 0 1))
  :element-type (integer 0 1))

(def-open-output-test open.output.7 ()
  (progn (close s) (with-open-file (is #p"tmp.dat"
				       :element-type 'bit)
				   (let ((seq (make-array 10)))
				     (read-sequence seq is)
				     seq)))
  (#(0 1 0 1 0 1 0 1 0 1))
  :element-type bit)

(def-open-output-test open.output.8 ()
  (progn (close s) (with-open-file (is #p"tmp.dat"
				       :element-type '(unsigned-byte 1))
				   (let ((seq (make-array 10)))
				     (read-sequence seq is)
				     seq)))
  (#(0 1 0 1 0 1 0 1 0 1))
  :element-type (unsigned-byte 1))

(def-open-output-test open.output.9 ()
  (progn (close s) (with-open-file (is #p"tmp.dat"
				       :element-type '(unsigned-byte 2))
				   (let ((seq (make-array 10)))
				     (read-sequence seq is)
				     seq)))
  (#(0 1 2 3 0 1 2 3 0 1))
  :element-type (unsigned-byte 2))

(def-open-output-test open.output.10 ()
  (progn (close s) (with-open-file (is #p"tmp.dat"
				       :element-type '(unsigned-byte 3))
				   (let ((seq (make-array 10)))
				     (read-sequence seq is)
				     seq)))
  (#(0 1 2 3 4 5 6 7 0 1))
  :element-type (unsigned-byte 3))

(def-open-output-test open.output.11 ()
  (progn (close s) (with-open-file (is #p"tmp.dat"
				       :element-type '(unsigned-byte 4))
				   (let ((seq (make-array 10)))
				     (read-sequence seq is)
				     seq)))
  (#(0 1 2 3 4 5 6 7 8 9))
  :element-type (unsigned-byte 4))


(def-open-output-test open.output.12 ()
  (progn (close s) (with-open-file (is #p"tmp.dat"
				       :element-type '(unsigned-byte 6))
				   (let ((seq (make-array 10)))
				     (read-sequence seq is)
				     seq)))
  (#(0 1 2 3 4 5 6 7 8 9))
  :element-type (unsigned-byte 6))

(def-open-output-test open.output.13 ()
  (progn (close s) (with-open-file (is #p"tmp.dat"
				       :element-type '(unsigned-byte 8))
				   (let ((seq (make-array 10)))
				     (read-sequence seq is)
				     seq)))
  (#(0 1 2 3 4 5 6 7 8 9))
  :element-type (unsigned-byte 8))

(def-open-output-test open.output.14 ()
  (progn (close s) (with-open-file (is #p"tmp.dat"
				       :element-type '(unsigned-byte 12))
				   (let ((seq (make-array 10)))
				     (read-sequence seq is)
				     seq)))
  (#(0 1 2 3 4 5 6 7 8 9))
  :element-type (unsigned-byte 12))

(def-open-output-test open.output.15 ()
  (progn (close s) (with-open-file (is #p"tmp.dat"
				       :element-type '(unsigned-byte 16))
				   (let ((seq (make-array 10)))
				     (read-sequence seq is)
				     seq)))
  (#(0 1 2 3 4 5 6 7 8 9))
  :element-type (unsigned-byte 16))

(def-open-output-test open.output.16 ()
  (progn (close s) (with-open-file (is #p"tmp.dat"
				       :element-type '(unsigned-byte 24))
				   (let ((seq (make-array 10)))
				     (read-sequence seq is)
				     seq)))
  (#(0 1 2 3 4 5 6 7 8 9))
  :element-type (unsigned-byte 24))

(def-open-output-test open.output.17 ()
  (progn (close s) (with-open-file (is #p"tmp.dat"
				       :element-type '(unsigned-byte 32))
				   (let ((seq (make-array 10)))
				     (read-sequence seq is)
				     seq)))
  (#(0 1 2 3 4 5 6 7 8 9))
  :element-type (unsigned-byte 32))

(def-open-output-test open.output.18 ()
  (progn (close s) (with-open-file (is #p"tmp.dat"
				       :element-type '(unsigned-byte 64))
				   (let ((seq (make-array 10)))
				     (read-sequence seq is)
				     seq)))
  (#(0 1 2 3 4 5 6 7 8 9))
  :element-type (unsigned-byte 64))

(def-open-output-test open.output.19 ()
  (progn (close s) (with-open-file (is #p"tmp.dat"
				       :element-type '(unsigned-byte 100))
				   (let ((seq (make-array 10)))
				     (read-sequence seq is)
				     seq)))
  (#(0 1 2 3 4 5 6 7 8 9))
  :element-type (unsigned-byte 100))
