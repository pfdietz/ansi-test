;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jun 27 06:29:39 2004
;;;; Contains: Tests of PPRINT-TABULAR

(in-package :cl-test)

;;; When printing a non-list, the result is the same as calling WRITE."
(deftest pprint-tabular.1
  (my-with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-readably* nil))
     (loop for obj in *mini-universe*
	   nconc
	   (and (not (listp obj))
		(let ((s1 (write-to-string obj))
		      (s2 (with-output-to-string (s) (pprint-tabular s obj))))
		  (unless (equal s1 s2)
		    (list (list obj s1 s2))))))))
  nil)

(deftest pprint-tabular.2
  (my-with-standard-io-syntax
   (let ((*print-pretty* nil)
	 (*print-readably* nil))
     (loop for obj in *mini-universe*
	   nconc
	   (and (not (listp obj))
		(let ((s1 (write-to-string obj))
		      (s2 (with-output-to-string (s) (pprint-tabular s obj))))
		  (unless (equal s1 s2)
		    (list (list obj s1 s2))))))))
  nil)

(defmacro def-pprint-tabular-test (name args expected-value &key (margin 100) (circle nil) (pre nil))
  `(deftest ,name
     (my-with-standard-io-syntax
      (let ((*print-pretty* t)
	    (*print-readably* nil)
	    (*print-right-margin* ,margin)
	    (*package* (find-package :cl-test))
	    (*print-circle* ,circle))
	(with-output-to-string
	  (s)
	  ,@(when pre (list pre))
	  (pprint-tabular s ,@args))))
     ,expected-value))

(def-pprint-tabular-test pprint-tabular.3 ('(|M|)) "(M)")
(def-pprint-tabular-test pprint-tabular.4 ('(|M|) t) "(M)")
(def-pprint-tabular-test pprint-tabular.5 ('(|M|) nil) "M")

(def-pprint-tabular-test pprint-tabular.6 ('(|M| |M|)) "(M              M)")
(def-pprint-tabular-test pprint-tabular.7 ('(|M| |M|) t nil 1) "(M M)")
(def-pprint-tabular-test pprint-tabular.8 ('(|M| |M|) t t 3) "(M M)")
(def-pprint-tabular-test pprint-tabular.9 ('(|M| |M|) t nil 4) "(M  M)")
(def-pprint-tabular-test pprint-tabular.10 ('(|MM| |MM|) t nil 4) "(MM MM)")
(def-pprint-tabular-test pprint-tabular.11 ('(|MM| |MM|) t nil 5) "(MM  MM)")
(def-pprint-tabular-test pprint-tabular.12 ('(|M| |MM|) t nil 5)  "(M   MM)")

(def-pprint-tabular-test pprint-tabular.13 ((let ((x (list '|A|))) (list x x)) t nil 1)
  "(#1=(A) #1#)" :circle t)

(def-pprint-tabular-test pprint-tabular.14 ('(|M| |M|) t t 4) "(M  M)")

(def-pprint-tabular-test pprint-tabular.15 ('(1 2 3 4) t t 1) "(1 2 3 4)")
(def-pprint-tabular-test pprint-tabular.16 ('(10 20 30 40) t t 1) "(10 20 30 40)")
(def-pprint-tabular-test pprint-tabular.17 ('(10 200 3000 40000) t t 1) "(10 200 3000 40000)")
(def-pprint-tabular-test pprint-tabular.18 ('(10 20 30 40) t t 2) "(10 20  30  40)")
(def-pprint-tabular-test pprint-tabular.19 ('(10 200 3000 40000) t t 2) "(10 200 3000  40000)")

(def-pprint-tabular-test pprint-tabular.20 ('(1 2 3) t nil 1)
  "     (1 2 3)"
  :pre (write "     " :stream s :escape nil))

(def-pprint-tabular-test pprint-tabular.21 ('(1 2 3) t nil 1)
  "     (1
      2
      3)"
  :pre (write "     " :stream s :escape nil) :margin 9)


(def-pprint-tabular-test pprint-tabular.22 ('(1 2 3) t nil 1)
  "     (1 2
      3)"
  :pre (write "     " :stream s :escape nil) :margin 10)


;;; Test that pprint-tabular returns NIL

(deftest pprint-tabular.return-values.1
  (my-with-standard-io-syntax
   (let ((*print-pretty* nil)
	 (*package* (find-package :cl-test)))
     (with-open-stream (s (make-broadcast-stream))
		       (pprint-tabular s '(a b)))))
  nil)

(deftest pprint-tabular.return-values.2
  (my-with-standard-io-syntax
   (let ((*print-pretty* nil)
	 (*package* (find-package :cl-test)))
     (with-open-stream (s (make-broadcast-stream))
		       (pprint-tabular s 10 nil nil 100))))
  nil)


