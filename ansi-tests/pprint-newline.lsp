;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Jul  7 07:48:01 2004
;;;; Contains: Tests of PPRINT-NEWLINE

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(defmacro def-pprint-newline-test (name form expected-value &rest key-args)
  `(def-pprint-test ,name
     (with-output-to-string
       (*standard-output*)
       (pprint-logical-block (*standard-output* nil) ,form))
     ,expected-value
     ,@key-args))

;;; NIL designates the standard output

(def-pprint-test pprint-newline.1
  (with-output-to-string
    (*standard-output*)
    (pprint-logical-block
     (*standard-output* nil)
     (dotimes (i 8)
       (write-char #\A)
       (write-char #\Space)
       (pprint-newline :fill nil))))
  "A A A A A
A A A "
  :margin 10)

;;; T designates the stream *terminal-io*
(def-pprint-test pprint-newline.2
  (with-output-to-string
    (os)
    (with-input-from-string
     (is "")
     (with-open-stream
      (*terminal-io* (make-two-way-stream is os))
      (pprint-logical-block
       (*terminal-io* nil)
       (dotimes (i 8)
	 (write "A " :stream t)
	 (pprint-newline :fill t))))))
  "A A A A A
A A A "
  :margin 10)

;;; No stream is standard output

(def-pprint-test pprint-newline.3
  (with-output-to-string
    (*standard-output*)
    (pprint-logical-block
     (*standard-output* nil)
     (dotimes (i 8)
       (write-char #\A)
       (write-char #\Space)
       (pprint-newline :fill))))
  "A A A A A
A A A "
  :margin 10)

;;; :linear

(def-pprint-newline-test pprint-newline.linear.1
  (progn
   (dotimes (i 2) (write "A ") (pprint-newline :fill))
   (write "B ") (pprint-newline :linear)
   (dotimes (i 3) (write "A ") (pprint-newline :fill)))
  "A A B
A A A "
  :margin 10)

(def-pprint-newline-test pprint-newline.linear.2
  (progn
   (dotimes (i 2) (write "A ") (pprint-newline :fill))
   (write "B ") (pprint-newline :linear)
   (dotimes (i 2) (write "C ") (pprint-newline :fill))
   (write "D ") (pprint-newline :linear)
   (dotimes (i 3) (write "A ") (pprint-newline :fill)))
  "A A B
C C D
A A A "
  :margin 10)

(def-pprint-newline-test pprint-newline.linear.3
  (dotimes (i 4) (write "A ") (pprint-newline :linear))
  "A A A A "
  :margin 10)

(def-pprint-newline-test pprint-newline.linear.4
  (dotimes (i 4) (write "A ") (pprint-newline :linear))
  "A A A A "
  :margin 10
  :miser 10)

(def-pprint-newline-test pprint-newline.linear.5
  (dotimes (i 10) (write "A ") (pprint-newline :linear))
  "A A A A A A A A A A "
  :margin 10
  :pretty nil)

(def-pprint-newline-test pprint-newline.linear.6
  (dotimes (i 4) (write "A             ") (pprint-newline :linear))
  "A
A
A
A
"
  :margin 10)

(def-pprint-newline-test pprint-newline.linear.7
  (progn
    (dotimes (i 4) (write "A ") (pprint-newline :linear))
    (terpri)
    (dotimes (i 4) (write "A ") (pprint-newline :linear)))
  "A
A
A
A

A
A
A
A
"
  :margin 10)

(def-pprint-newline-test pprint-newline.linear.8
  (progn
    (pprint-logical-block (*standard-output* nil)
			  (dotimes (i 4)
			    (write "A ")
			    (pprint-newline :linear)))
    (pprint-newline :linear)
    (pprint-logical-block (*standard-output* nil)
			  (dotimes (i 4)
			    (write "A ")
			    (pprint-newline :linear))))
  "A A A A
A A A A "
  :margin 10)
    
(def-pprint-newline-test pprint-newline.linear.9
  (dotimes (i 10) (write "A ") (let ((*print-pretty* nil)) (pprint-newline :linear)))
  "A A A A A A A A A A "
  :margin 10)

;;; :miser

(def-pprint-newline-test pprint-newline.miser.1
  (dotimes (i 10) (write "A ") (pprint-newline :miser))
  "A A A A A A A A A A "
  :margin 10)

(def-pprint-newline-test pprint-newline.miser.2
  (dotimes (i 10) (write "A ") (pprint-newline :miser))
  "A A A A A A A A A A "
  :margin 10
  :miser 0)

(def-pprint-newline-test pprint-newline.miser.3
  (dotimes (i 10) (write "A ") (pprint-newline :miser))
  "A A A A A A A A A A "
  :margin 10
  :miser 9)

(def-pprint-newline-test pprint-newline.miser.4
  (dotimes (i 10) (write "A ") (pprint-newline :miser))
  "A
A
A
A
A
A
A
A
A
A
"
  :margin 10
  :miser 10)

(def-pprint-newline-test pprint-newline.miser.5
  (dotimes (i 10) (write "A ") (pprint-newline :miser))
  "A A A A A A A A A A "
  :margin 10
  :miser 10
  :pretty nil)

(def-pprint-newline-test pprint-newline.miser.6
  (progn
    (terpri)
    (write "A")
    (pprint-newline :miser))
  "
A
"
  :margin 20
  :miser 20)

(def-pprint-newline-test pprint-newline.miser.7
  (progn
    (pprint-newline :miser)
    (write "A")
    (terpri))
  "
A
"
  :margin 20
  :miser 20)

