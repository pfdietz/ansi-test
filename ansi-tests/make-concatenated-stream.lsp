;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Feb 14 08:41:18 2004
;;;; Contains: Tests of MAKE-CONCATENATED-STREAM

(in-package :cl-test)

(deftest make-concatenated-stream.1
  (let ((s (make-concatenated-stream)))
    (read s nil :eof))
  :eof)

(deftest make-concatenated-stream.2
  (let ((s (make-concatenated-stream)))
    (notnot-mv (input-stream-p s)))
  t)

(deftest make-concatenated-stream.3
  (let ((s (make-concatenated-stream)))
    (output-stream-p s))
  nil)

(deftest make-concatenated-stream.4
  (let ((s (make-concatenated-stream)))
    (notnot-mv (streamp s)))
  t)

(deftest make-concatenated-stream.5
  (let ((s (make-concatenated-stream)))
    (notnot-mv (typep s 'stream)))
  t)

(deftest make-concatenated-stream.6
  (let ((s (make-concatenated-stream)))
    (notnot-mv (typep s 'concatenated-stream)))
  t)

(deftest make-concatenated-stream.7
  (let ((s (make-concatenated-stream)))
    (notnot-mv (open-stream-p s)))
  t)

(deftest make-concatenated-stream.8
  (let ((s (make-concatenated-stream *standard-input*)))
    (notnot-mv (stream-element-type s)))
  t)

(deftest make-concatenated-stream.9
  (let ((pn #p"tmp.dat")
	(element-type '(unsigned-byte 8)))
    (with-open-file (s pn :direction :output :element-type element-type
		       :if-exists :supersede)
		    (dolist (b '(1 5 9 13)) (write-byte b s)))
    (with-open-file
     (s1 pn :direction :input :element-type element-type)
     (with-open-file
      (s2 pn :direction :input :element-type element-type)
      (let ((s (make-concatenated-stream s1 s2)))
	(loop repeat 8 collect (read-byte s))))))
  (1 5 9 13 1 5 9 13))

(deftest make-concatenated-stream.10
  (let ((s (make-concatenated-stream)))
    (read-byte s nil :eof))
  :eof)

(deftest make-concatenated-stream.11
  (let ((s (make-concatenated-stream)))
    (peek-char nil s nil :eof))
  :eof)

(deftest make-concatenated-stream.12
  (with-input-from-string
   (s1 "a")
   (with-input-from-string
    (s2 "b")
    (let ((s (make-concatenated-stream s1 s2)))
      (values
       (peek-char nil s)
       (read-char s)
       (peek-char nil s)
       (read-char s)
       (peek-char nil s nil :eof)))))
  #\a #\a #\b #\b :eof)

(deftest make-concatenated-stream.13
  (with-input-from-string
   (s1 "  a  ")
   (with-input-from-string
    (s2 "  b  ")
    (let ((s (make-concatenated-stream s1 s2)))
      (values
       (peek-char t s)
       (read-char s)
       (peek-char t s)
       (read-char s)
       (peek-char t s nil :eof)))))
  #\a #\a #\b #\b :eof)

(deftest make-concatenated-stream.14
  (with-input-from-string
   (s1 "a")
   (with-input-from-string
    (s2 "b")
    (let ((s (make-concatenated-stream s1 s2)))
      (values
       (read-char s)
       (unread-char #\a s)
       (read-char s)
       (read-char s)
       (unread-char #\b s)
       (read-char s)
       (read-char s nil :eof)))))
  #\a nil #\a #\b nil #\b :eof)

(deftest make-concatenated-stream.15
  (let ((s (make-concatenated-stream)))
    (read-char-no-hang s nil :eof))
  :eof)

(deftest make-concatenated-stream.16
  (with-input-from-string
   (s1 "a")
   (with-input-from-string
    (s2 "b")
    (let ((s (make-concatenated-stream s1 s2)))
      (values
       (read-char-no-hang s)
       (read-char-no-hang s)
       (read-char-no-hang s nil :eof)))))
  #\a #\b :eof)

;;; Error cases

(deftest make-concatenated-stream.error.1
  (loop for x in *mini-universe*
	unless (or (and (streamp x) (input-stream-p x))
		   (eval `(signals-error (make-concatenated-stream ',x) t)))
	collect x)
  nil)

(deftest make-concatenated-stream.error.2
  (loop for x in *streams*
	unless (or (and (streamp x) (input-stream-p x))
		   (eval `(signals-error (make-concatenated-stream ',x) t)))
	collect x)
  nil)

