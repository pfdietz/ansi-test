;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Jun 25 22:03:01 2004
;;;; Contains: Tests of PPRINT-FILL

(in-package :cl-test)

;;; When printing a non-list, the result is the same as calling WRITE."
(deftest pprint-fill.1
  (my-with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-readably* nil))
     (loop for obj in *mini-universe*
	   nconc
	   (and (not (listp obj))
		(let ((s1 (write-to-string obj))
		      (s2 (with-output-to-string (s) (assert (null (pprint-fill s obj))))))
		  (unless (equal s1 s2)
		    (list (list obj s1 s2))))))))
  nil)

(deftest pprint-fill.2
  (my-with-standard-io-syntax
   (let ((*print-pretty* nil)
	 (*print-readably* nil))
     (loop for obj in *mini-universe*
	   nconc
	   (and (not (listp obj))
		(let ((s1 (write-to-string obj))
		      (s2 (with-output-to-string (s) (assert (null (pprint-fill s obj))))))
		  (unless (equal s1 s2)
		    (list (list obj s1 s2))))))))
  nil)

(deftest pprint-fill.3
  (my-with-standard-io-syntax
   (let ((*print-pretty* nil)
	 (*print-readably* nil)
	 (*print-right-margin* 100))
     (with-output-to-string
       (s)
       (assert (null (pprint-fill s '(cl-user::|A|)))))))
  "(A)")

(deftest pprint-fill.4
  (my-with-standard-io-syntax
   (let ((*print-pretty* nil)
	 (*print-readably* nil)
	 (*print-right-margin* 100))
     (with-output-to-string
       (s)
       (assert (null (pprint-fill s '(cl-user::|A|) t))))))
  "(A)")

(deftest pprint-fill.5
  (my-with-standard-io-syntax
   (let ((*print-pretty* nil)
	 (*print-readably* nil)
	 (*print-right-margin* 100))
     (with-output-to-string
       (s)
       (assert (null (pprint-fill s '(cl-user::|A|) nil))))))
  "A")

(deftest pprint-fill.6
  (my-with-standard-io-syntax
   (let ((*print-pretty* nil)
	 (*print-readably* nil)
	 (*print-right-margin* 100))
     (with-output-to-string
       (s)
       (assert (null (pprint-fill s '(1 2 3 4 5)))))))
  "(1 2 3 4 5)")

(deftest pprint-fill.7
  (my-with-standard-io-syntax
   (let ((*print-pretty* nil)
	 (*print-readably* nil)
	 (*print-right-margin* 100))
     (with-output-to-string
       (s)
       (assert (null (pprint-fill s '((1) (2) #(3) "abc" 5) nil))))))
  "(1) (2) #(3) \"abc\" 5")









