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

;;; More tests of stream functions on echo streams go here

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




