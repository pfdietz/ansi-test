;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Jan 28 06:54:33 2004
;;;; Contains: Tests of MAKE-SYNONYM-STREAM

(in-package :cl-test)

(deftest make-synonym-stream.1
  (with-input-from-string
   (*s* "abcde")
   (declare (special *s*))
   (let ((ss (make-synonym-stream '*s*)))
     (assert (typep ss 'stream))
     (assert (typep ss 'synonym-stream))
     (assert (input-stream-p ss))
     (values
      (read-char *s*)
      (read-char ss)
      (read-char *s*)
      (read-char ss)
      (read-char ss))))
  #\a #\b #\c #\d #\e)


(deftest make-synonym-stream.2
   (let ((ss (make-synonym-stream '*s*)))
     (with-input-from-string
      (*s* "z")
      (declare (special *s*))
      (assert (typep ss 'stream))
      (assert (typep ss 'synonym-stream))
      (assert (input-stream-p ss))
      (read-char ss)))
   #\z)

(deftest make-synonym-stream.3
  (with-output-to-string
   (*s*)
   (declare (special *s*))
   (let ((ss (make-synonym-stream '*s*)))
     (assert (typep ss 'stream))
     (assert (typep ss 'synonym-stream))
     (assert (output-stream-p ss))
     (write-char #\a *s*)
     (write-char #\b ss)
     (write-char #\x *s*)
     (write-char #\y ss)))
  "abxy")

;;; FIXME
;;; Add tests for: open-stream-p, close, stream-element-type, streamp,
;;;  peek-char, read-char-no-hang, terpri, fresh-line, unread-char,
;;;  read-line, write-line, write-string, read-sequence, write-sequence,
;;;  read-byte, write-byte, listen, clear-input, finish-output, force-output,
;;;  clear-output, format, print, prin1, princ

;;; Error cases

(deftest make-synonym-stream.error.1
  (signals-error (make-synonym-stream) program-error)
  t)

(deftest make-synonym-stream.error.2
  (signals-error (make-synonym-stream '*standard-input* nil) program-error)
  t)

(deftest make-synonym-stream.error.3
  (loop for x in *mini-universe*
	unless (or (symbolp x)
		   (eval `(signals-error (make-synonym-stream ',x)
					 type-error)))
	collect x)
  nil)
