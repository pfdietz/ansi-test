;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Aug  2 01:42:35 2004
;;;; Contains: Tests of printing using the ~A directive

(in-package :cl-test)

(deftest format.a.1
  (with-standard-io-syntax
   (format nil "~a" nil))
  "NIL")

(deftest format.a.2
  (with-standard-io-syntax
   (let ((*print-case* :downcase))
     (format nil "~A" nil)))
  "nil")

(deftest format.a.3
  (with-standard-io-syntax
   (let ((*print-case* :capitalize))
     (format nil "~a" nil)))
  "Nil")

(deftest format.a.4
  (format nil "~:a" nil)
  "()")

(deftest format.a.5
  (with-standard-io-syntax
   (format nil "~:A" '(nil)))
  "(NIL)")

(deftest format.a.6
  (with-standard-io-syntax
   (format nil "~:A" #(nil)))
  "#(NIL)")

(deftest format.a.7
  (loop for c across +standard-chars+
	for s1 = (string c)
	for s2 = (format nil "~a" s1)
	unless (string= s1 s2)
	collect (list c s1 s2))
  nil)

(deftest format.a.8
  (loop with count = 0
	for i from 0 below (min #x10000 char-code-limit)
	for c = (code-char i)
	for s1 = (and c (string c))
	for s2 = (and c (format nil "~A" s1))
	unless (or (null c) (string= s1 s2))
	do (incf count) and collect (list c s1 s2)
	when (> count 100) collect "count limit exceeded" and do (loop-finish))
  nil)

(deftest format.a.9
  (with-standard-io-syntax
   (apply
    #'values
    (loop for i from 1 to 10
	  for s1 = (format nil "~~~d@a" i)
	  for s2 = (format nil s1 nil)
	  collect s2)))
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

(deftest format.a.10
  (with-standard-io-syntax
   (apply
    #'values
    (loop for i from 1 to 10
	  for s1 = (format nil "~~~da" i)
	  for s2 = (format nil s1 nil)
	  collect s2)))
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

(deftest format.a.11
  (with-standard-io-syntax
   (apply
    #'values
    (loop for i from 1 to 10
	  for s1 = (format nil "~~~d@:A" i)
	  for s2 = (format nil s1 nil)
	  collect s2)))
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

(deftest format.a.12
  (with-standard-io-syntax
   (apply
    #'values
    (loop for i from 1 to 10
	  for s1 = (format nil "~~~d:a" i)
	  for s2 = (format nil s1 nil)
	  collect s2)))
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

(deftest format.a.13
  (with-standard-io-syntax
   (apply
    #'values
    (loop for i from 1 to 10
	  for s2 = (format nil "~v:A" i nil)
	  collect s2)))
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

(deftest format.a.14
  (with-standard-io-syntax
   (apply
    #'values
    (loop for i from 1 to 10
	  for s2 = (format nil "~v:@a" i nil)
	  collect s2)))
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

(deftest format.a.15
  (with-standard-io-syntax
   (format nil "~vA" nil nil))
  "NIL")

(deftest format.a.16
  (with-standard-io-syntax
   (format nil "~v:A" nil nil))
  "()")

(deftest format.a.17
  (with-standard-io-syntax
   (format nil "~@A" nil))
  "NIL")

(deftest format.a.18
  (with-standard-io-syntax
   (format nil "~v@A" nil nil))
  "NIL")

(deftest format.a.19
  (with-standard-io-syntax
   (format nil "~v:@a" nil nil))
  "()")

(deftest format.a.20
  (with-standard-io-syntax
   (format nil "~v@:a" nil nil))
  "()")

;;; With colinc specified

(deftest format.a.21
  (format nil "~3,1a" nil)
  "NIL")

(deftest format.a.22
  (format nil "~4,3a" nil)
  "NIL   ")

(deftest format.a.23
  (format nil "~3,3@a" nil)
  "NIL")

(deftest format.a.24
  (format nil "~4,4@a" nil)
  "    NIL")

(deftest format.a.25
  (format nil "~5,3@a" nil)
  "   NIL")

(deftest format.a.26
  (format nil "~5,3A" nil)
  "NIL   ")

(deftest format.a.27
  (format nil "~7,3@a" nil)
  "      NIL")

(deftest format.a.28
  (format nil "~7,3A" nil)
  "NIL      ")

;;; With minpad

(deftest format.a.29
  (loop for i from -4 to 10
	collect (format nil "~v,,2A" i "ABC"))
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

(deftest format.a.30
  (format nil "~3,,+2A" "ABC")
  "ABC  ")

(deftest format.a.31
  (format nil "~3,,0A" "ABC")
  "ABC")

(deftest format.a.32
  (format nil "~3,,-1A" "ABC")
  "ABC")

(deftest format.a.33
  (format nil "~3,,0A" "ABCD")
  "ABCD")

(deftest format.a.34
  (format nil "~3,,-1A" "ABCD")
  "ABCD")

;;; With padchar

(deftest format.a.35
  (format nil "~4,,,'XA" "AB")
  "ABXX")

(deftest format.a.36
  (format nil "~4,,,a" "AB")
  "AB  ")

(deftest format.a.37
  (format nil "~4,,,'X@a" "AB")
  "XXAB")

(deftest format.a.38
  (format nil "~4,,,@A" "AB")
  "  AB")

(deftest format.a.39
  (format nil "~10,,,vA" nil "abcde")
  "abcde     ")

(deftest format.a.40
  (format nil "~10,,,v@A" nil "abcde")
  "     abcde")

(deftest format.a.41
  (format nil "~10,,,va" #\* "abcde")
  "abcde*****")

(deftest format.a.42
  (format nil "~10,,,v@a" #\* "abcde")
  "*****abcde")

;;; Other tests

(deftest format.a.43
  (format nil "~3,,vA" nil "ABC")
  "ABC")

(deftest format.a.44
  (loop for i from 0 to 6
	collect (format nil "~3,,vA" i "ABC"))
  ("ABC"
   "ABC "
   "ABC  "
   "ABC   "
   "ABC    "
   "ABC     "
   "ABC      "))

(deftest format.a.44a
  (loop for i from 0 to 6
	collect (format nil "~3,,v@A" i "ABC"))
  ("ABC"
   " ABC"
   "  ABC"
   "   ABC"
   "    ABC"
   "     ABC"
   "      ABC"))

(deftest format.a.45
  (format nil "~4,,va" -1 "abcd")
  "abcd")

(deftest format.a.46
  (format nil "~5,vA" nil "abc")
  "abc  ")

(deftest format.a.47
  (format nil "~5,vA" 3 "abc")
  "abc   ")

(deftest format.a.48
  (format nil "~5,v@A" 3 "abc")
  "   abc")

;;; # parameters

(deftest format.a.49
  (format nil "~#A" "abc" nil nil nil)
  "abc ")

(deftest format.a.50
  (format nil "~#@a" "abc" nil nil nil nil nil)
  "   abc")

(deftest format.a.51
  (format nil "~5,#a" "abc" nil nil nil)
  "abc    ")

(deftest format.a.52
  (format nil "~5,#@A" "abc" nil nil nil)
  "    abc")

(deftest format.a.53
  (format nil "~4,#A" "abc" nil nil)
  "abc   ")

(deftest format.a.54
  (format nil "~4,#@A" "abc" nil nil)
  "   abc")

(deftest format.a.55
  (format nil "~#,#A" "abc" nil nil nil)
  "abc    ")

(deftest format.a.56
  (format nil "~#,#@A" "abc" nil nil nil)
  "    abc")

(deftest format.a.57
  (format nil "~-100A" "xyz")
  "xyz")

(deftest format.a.58
  (format nil "~-100000000000000000000a" "xyz")
  "xyz")
