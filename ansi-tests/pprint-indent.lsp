;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jul  3 08:50:40 2004
;;;; Contains: Tests of PPRINT-INDENT

(in-package :cl-test)

(deftest pprint-indent.1
  (with-standard-io-syntax
   (let ((*print-pretty* nil))
     (with-open-stream (*standard-output* (make-string-output-stream))
		       (pprint-indent :block 0))))
  nil)

(deftest pprint-indent.2
  (with-standard-io-syntax
   (let ((*print-pretty* nil))
     (with-open-stream (*standard-output* (make-broadcast-stream))
		       (pprint-indent :current 0))))
  nil)

(deftest pprint-indent.3
  (with-standard-io-syntax
   (let ((*print-pretty* nil))
     (with-open-stream (s (make-string-output-stream))
		       (pprint-indent :current 10 s))))
  nil)

(deftest pprint-indent.4
  (with-standard-io-syntax
   (let ((*print-pretty* nil))
     (with-open-stream (s (make-string-output-stream))
		       (pprint-indent :block 1/2 s))))
  nil)

(deftest pprint-indent.5
  (with-standard-io-syntax
   (let ((*print-pretty* nil))
     (with-open-stream (s (make-string-output-stream))
		       (pprint-indent :block 0.1 s))))
  nil)

(deftest pprint-indent.6
  (with-standard-io-syntax
   (let ((*print-pretty* nil))
     (loop for x in '(1.0s0 1.0f0 1.0d0 1.0l0)
	   unless
	   (equal
	    (multiple-value-list
	     (with-open-stream (s (make-string-output-stream))
			       (pprint-indent :block x s)))
	    '(nil))
	   collect x)))
  nil)

(deftest pprint-indent.7
  (with-standard-io-syntax
   (let ((*print-pretty* nil))
     (with-open-stream (*standard-output* (make-broadcast-stream))
		       (pprint-indent :current 0 nil))))
  nil)

(deftest pprint-indent.8
  (with-standard-io-syntax
   (let ((*print-pretty* nil))
     (with-open-stream
      (os (make-string-output-stream))
      (with-open-stream
       (is (make-string-input-stream ""))
       (with-open-stream (*terminal-io* (make-two-way-stream is os))
			 (pprint-indent :current 0 t))))))
  nil)

;;; Now test with pprint-logical-block

(deftest pprint-indent.9
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-readably* nil)
	 (*print-right-margin* 100)
	 (*print-escape* nil)
	 (*print-case* :upcase)
	 (*print-miser-width* nil)
	 (*print-circle* nil))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os '(|M| |M|))
	(write '|M| :stream os)
	(pprint-indent :current 3 os)
	(pprint-newline :mandatory os)
	(write '|M| :stream os)))))
  "M
    M")

(deftest pprint-indent.10
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-readably* nil)
	 (*print-right-margin* 100)
	 (*print-escape* nil)
	 (*print-case* :upcase)
	 (*print-miser-width* nil)
	 (*print-circle* nil))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os '(|M| |M|) :prefix "(" :suffix ")")
	(write '|M| :stream os)
	(pprint-indent :current 1 os)
	(pprint-newline :mandatory os)
	(write '|M| :stream os)))))
  "(M
   M)")


			     