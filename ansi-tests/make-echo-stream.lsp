;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Feb 12 04:34:42 2004
;;;; Contains: Tests of MAKE-ECHO-STREAM

(in-package :cl-test)

(deftest make-echo-stream.1
  (let* ((is (make-string-input-stream "foo"))
	 (os (make-string-output-stream))
	 (s (make-echo-stream is os)))
    (values
     (read-char s)
     (get-output-stream-string os)))
  #\f "f")

(deftest make-echo-stream.2
  (let* ((is (make-string-input-stream "foo"))
	 (os (make-string-output-stream))
	 (s (make-echo-stream is os)))
    (get-output-stream-string os))
  "")

(deftest make-echo-stream.3
  (let* ((is (make-string-input-stream "foo"))
	 (os (make-string-output-stream))
	 (s (make-echo-stream is os)))
    (values (read-line s nil)
	    (get-output-stream-string os)))
  "foo" "foo")

;;; Tests of READ-BYTE on echo streams

(deftest make-echo-stream.4
  (let ((pn #p"tmp.dat")
	(pn2 #p"tmp2.dat")
	(element-type '(unsigned-byte 8)))
    (with-open-file (os pn
			:direction :output
			:element-type element-type
			:if-exists :supersede)
		    (loop for x in '(2 3 5 7 11)
			  do (write-byte x os)))
    (with-open-file
     (is pn :direction :input :element-type element-type)
     (values
      (with-open-file
       (os pn2 :direction :output :if-exists :supersede
	   :element-type element-type)
       (let ((s (make-echo-stream is os)))
	 (loop repeat 6 collect (read-byte s nil :eof1))))
      (with-open-file
       (s pn2 :direction :input :element-type element-type)
       (loop repeat 6 collect (read-byte s nil :eof2))))))
  (2 3 5 7 11 :eof1)
  (2 3 5 7 11 :eof2))

(deftest make-echo-stream.5
  (let ((pn #p"tmp.dat")
	(pn2 #p"tmp2.dat")
	(element-type '(unsigned-byte 8)))
    (with-open-file (os pn
			:direction :output
			:element-type element-type
			:if-exists :supersede)
		    (loop for x in '(2 3 5 7 11)
			  do (write-byte x os)))
    (with-open-file
     (is pn :direction :input :element-type element-type)
     (values
      (with-open-file
       (os pn2 :direction :output :if-exists :supersede
	   :element-type element-type)
       (let ((s (make-echo-stream is os)))
	 (loop repeat 6 collect (read-byte s nil 100))))
      (with-open-file
       (s pn2 :direction :input :element-type element-type)
       (loop repeat 6 collect (read-byte s nil 200))))))
  (2 3 5 7 11 100)
  (2 3 5 7 11 200))

(deftest make-echo-stream.6
  (let* ((is (make-string-input-stream "foo"))
	 (os (make-string-output-stream))
	 (s (make-echo-stream is os)))
    (values (coerce (loop repeat 3 collect (read-char-no-hang s)) 'string)
	    (get-output-stream-string os)))
  "foo" "foo")

(deftest make-echo-stream.7
  (let* ((is (make-string-input-stream "foo"))
	 (os (make-string-output-stream))
	 (s (make-echo-stream is os)))
    (values (coerce (loop repeat 4 collect (read-char-no-hang s nil '#\z))
		    'string)
	    (get-output-stream-string os)))
  "fooz" "foo")

;;; peek-char + echo streams is tested in peek-char.lsp

(deftest make-echo-stream.8
  (let* ((is (make-string-input-stream "foo"))
	 (os (make-string-output-stream))
	 (s (make-echo-stream is os))
	 (x (copy-seq "xxxxxx")))
    (values
     (read-sequence x s)
     x
     (get-output-stream-string os)))
  3
  "fooxxx"
  "foo")

(deftest make-echo-stream.9
  (let ((pn #p"tmp.dat")
	(pn2 #p"tmp2.dat")
	(element-type '(unsigned-byte 8)))
    (with-open-file (os pn
			:direction :output
			:element-type element-type
			:if-exists :supersede)
		    (loop for x in '(2 3 5 7 11)
			  do (write-byte x os)))
    (with-open-file
     (is pn :direction :input :element-type element-type)
     (values
      (with-open-file
       (os pn2 :direction :output :if-exists :supersede
	   :element-type element-type)
       (let ((s (make-echo-stream is os))
	     (x (vector 0 0 0 0 0 0 0 0)))
	 (list (read-sequence x s)
	       x)))
      (with-open-file
       (s pn2 :direction :input :element-type element-type)
       (loop repeat 8 collect (read-byte s nil nil))))))
  (5 #(2 3 5 7 11 0 0 0))
  (2 3 5 7 11 nil nil nil))
    
;;; Error tests

(deftest make-echo-stream.error.1
  (signals-error (make-echo-stream) program-error)
  t)

(deftest make-echo-stream.error.2
  (signals-error (make-echo-stream *standard-input*) program-error)
  t)

(deftest make-echo-stream.error.3
  (signals-error (make-echo-stream *standard-input* *standard-output* nil)
		 program-error)
  t)




