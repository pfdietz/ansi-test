;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Aug  3 11:55:07 2004
;;;; Contains: Test of the ~S format directive

(in-package :cl-test)

(deftest format.s.1
  (let ((*print-readably* nil)
	(*print-case* :upcase))
    (format nil "~s" nil))
  "NIL")

(deftest format.s.2
  (let ((*print-readably* nil))
    (format nil "~:s" nil))
  "()")

(deftest format.s.3
  (let ((*print-readably* nil)
	(*print-case* :upcase))
    (format nil "~:s" '(nil)))
  "(NIL)")

(deftest format.s.4
  (let ((*print-readably* nil)
	(*print-case* :downcase))
    (format nil "~s" 'nil))
  "nil")

(deftest format.s.5
  (let ((*print-readably* nil)
	(*print-case* :capitalize))
    (format nil "~s" 'nil))
  "Nil")

(deftest format.s.6
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (format nil "~:s" #(nil))))
  "#(NIL)")

(deftest format.s.7
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (loop for c across +standard-chars+
	   for s = (format nil "~S" c)
	   for c2 = (read-from-string s)
	   unless (eql c c2)
	   collect (list c s c2))))
  nil)

(deftest format.s.8
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (loop with count = 0
	   for i from 0 below (min #x10000 char-code-limit)
	   for c = (code-char i)
	   for s1 = (and c (format nil "#\\~:c" c))
	   for s2 = (and c (format nil "~S" c))
	   unless (or (null c)
		      (graphic-char-p c)
		      (string= s1 s2))
	   do (incf count) and collect (list c s1 s2)
	   when (> count 100) collect "count limit exceeded" and do (loop-finish))))
  nil)

(deftest format.s.9
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (apply
      #'values
      (loop for i from 1 to 10
	    for s1 = (format nil "~~~d@s" i)
	    for s2 = (format nil s1 nil)
	    collect s2))))
  "NIL"
  "NIL"
  "NIL"
  " NIL"
  "  NIL"
  "   NIL"
  "    NIL"
  "     NIL"
  "      NIL"
  "       NIL")

(deftest format.s.10
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (apply
      #'values
      (loop for i from 1 to 10
	    for s1 = (format nil "~~~dS" i)
	    for s2 = (format nil s1 nil)
	    collect s2))))
  "NIL"
  "NIL"
  "NIL"
  "NIL "
  "NIL  "
  "NIL   "
  "NIL    "
  "NIL     "
  "NIL      "
  "NIL       ")

(deftest format.s.11
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (apply
      #'values
      (loop for i from 1 to 10
	    for s1 = (format nil "~~~d@:S" i)
	    for s2 = (format nil s1 nil)
	    collect s2))))
  "()"
  "()"
  " ()"
  "  ()"
  "   ()"
  "    ()"
  "     ()"
  "      ()"
  "       ()"
  "        ()")

(deftest format.s.12
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (apply
      #'values
      (loop for i from 1 to 10
	    for s1 = (format nil "~~~d:s" i)
	    for s2 = (format nil s1 nil)
	    collect s2))))
  "()"
  "()"
  "() "
  "()  "
  "()   "
  "()    "
  "()     "
  "()      "
  "()       "
  "()        ")

(deftest format.s.13
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (apply
      #'values
      (loop for i from 1 to 10
	    for s2 = (format nil "~v:S" i nil)
	    collect s2))))
  "()"
  "()"
  "() "
  "()  "
  "()   "
  "()    "
  "()     "
  "()      "
  "()       "
  "()        ")

(deftest format.s.14
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (apply
      #'values
      (loop for i from 1 to 10
	    for s2 = (format nil "~v:@s" i nil)
	    collect s2))))
  "()"
  "()"
  " ()"
  "  ()"
  "   ()"
  "    ()"
  "     ()"
  "      ()"
  "       ()"
  "        ()")

(deftest format.s.15
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (format nil "~vS" nil nil)))
  "NIL")

(deftest format.s.16
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (format nil "~v:S" nil nil)))
  "()")

(deftest format.s.17
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (format nil "~@S" nil)))
  "NIL")

(deftest format.s.18
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (format nil "~v@S" nil nil)))
  "NIL")

(deftest format.s.19
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (format nil "~v:@s" nil nil)))
  "()")

(deftest format.s.20
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (format nil "~v@:s" nil nil)))
  "()")

;;; With colinc specified

(deftest format.s.21
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (format nil "~3,1s" nil)))
  "NIL")

(deftest format.s.22
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (format nil "~4,3s" nil)))
  "NIL   ")

(deftest format.s.23
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (format nil "~3,3@s" nil)))
  "NIL")

(deftest format.s.24
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (format nil "~4,4@s" nil)))
  "    NIL")

(deftest format.s.25
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (format nil "~5,3@s" nil)))
  "   NIL")

(deftest format.s.26
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (format nil "~5,3S" nil)))
  "NIL   ")

(deftest format.s.27
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (format nil "~7,3@s" nil)))
  "      NIL")

(deftest format.s.28
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (format nil "~7,3S" nil)))
  "NIL      ")

;;; With minpad

(deftest format.s.29
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*package* (find-package :cl-test)))
     (loop for i from -4 to 10
	   collect (format nil "~v,,2S" i 'ABC))))
  ("ABC  "
   "ABC  "
   "ABC  "
   "ABC  "
   "ABC  "
   "ABC  "
   "ABC  "
   "ABC  "
   "ABC  "
   "ABC  "
   "ABC   "
   "ABC    "
   "ABC     "
   "ABC      "
   "ABC       "))

(deftest format.s.30
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*package* (find-package :cl-test)))
     (format nil "~3,,+2S" 'ABC)))
  "ABC  ")

(deftest format.s.31
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*package* (find-package :cl-test)))
     (format nil "~3,,0S" 'ABC)))
  "ABC")

(deftest format.s.32
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*package* (find-package :cl-test)))
     (format nil "~3,,-1S" 'ABC)))
  "ABC")

(deftest format.s.33
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*package* (find-package :cl-test)))
     (format nil "~3,,0S" 'ABCD)))
  "ABCD")

(deftest format.s.34
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*package* (find-package :cl-test)))
     (format nil "~3,,-1S" 'ABCD)))
  "ABCD")

;;; With padchar

(deftest format.s.35
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*package* (find-package :cl-test)))
     (format nil "~4,,,'XS" 'AB)))
  "ABXX")

(deftest format.s.36
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*package* (find-package :cl-test)))
     (format nil "~4,,,s" 'AB)))
  "AB  ")

(deftest format.s.37
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*package* (find-package :cl-test)))
     (format nil "~4,,,'X@s" 'AB)))
  "XXAB")

(deftest format.s.38
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*package* (find-package :cl-test)))
     (format nil "~4,,,@S" 'AB)))
  "  AB")

(deftest format.s.39
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*package* (find-package :cl-test)))
     (format nil "~10,,,vS" nil 'ABCDE)))
  "ABCDE     ")

(deftest format.s.40
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*package* (find-package :cl-test)))
     (format nil "~10,,,v@S" nil 'ABCDE)))
  "     ABCDE")

(deftest format.s.41
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*package* (find-package :cl-test)))
     (format nil "~10,,,vs" #\* 'ABCDE)))
  "ABCDE*****")

(deftest format.s.42
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*package* (find-package :cl-test)))
     (format nil "~10,,,v@s" #\* 'ABCDE)))
  "*****ABCDE")

;;; Other tests

(deftest format.s.43
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (format nil "~3,,vS" nil 246)))
  "246")

(deftest format.s.44
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*package* (find-package :cl-test)))
     (loop for i from 0 to 6
	   collect (format nil "~3,,vS" i 'ABC))))
  ("ABC"
   "ABC "
   "ABC  "
   "ABC   "
   "ABC    "
   "ABC     "
   "ABC      "))

(deftest format.s.44a
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*package* (find-package :cl-test)))
     (loop for i from 0 to 6
	   collect (format nil "~3,,v@S" i 'ABC))))
  ("ABC"
   " ABC"
   "  ABC"
   "   ABC"
   "    ABC"
   "     ABC"
   "      ABC"))

(deftest format.s.45
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (format nil "~4,,vs" -1 1234)))
  "1234")

(deftest format.s.46
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (format nil "~5,vS" nil 123)))
  "123  ")

(deftest format.s.47
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (format nil "~5,vS" 3 456)))
  "456   ")

(deftest format.s.48
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (format nil "~5,v@S" 3 789)))
  "   789")
