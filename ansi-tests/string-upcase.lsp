;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Oct  1 07:51:00 2002
;;;; Contains: Tests for STRING-UPCASE

(in-package :cl-test)

(deftest string-upcase.1
  (let ((s "a"))
    (values (string-upcase s) s))
  "A" "a")

(deftest string-upcase.2
  (let ((s "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))
    (values (string-upcase s) s))
  "ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

(deftest string-upcase.3
  (let ((s "0123456789!@#$%^&*()_-+=|\\{}[]:\";'<>?,./ "))
    (values (string-upcase s) s))
  "0123456789!@#$%^&*()_-+=|\\{}[]:\";'<>?,./ "
  "0123456789!@#$%^&*()_-+=|\\{}[]:\";'<>?,./ ")

(deftest string-upcase.4
  (string-upcase #\a)
  "A")

(deftest string-upcase.5
  (let ((sym '|a|))
    (values (string-upcase sym) sym))
  "A" |a|)

(deftest string-upcase.6
  (let ((s (make-array 6 :element-type 'character
		       :initial-contents '(#\a #\b #\c #\d #\e #\f))))
    (values (string-upcase s) s))
  "ABCDEF"
  "abcdef")

(deftest string-upcase.7
  (let ((s (make-array 6 :element-type 'standard-char
		       :initial-contents '(#\a #\b #\7 #\d #\e #\f))))
    (values (string-upcase s) s))
  "AB7DEF"
  "ab7def")

;; Tests with :start, :end

(deftest string-upcase.8
  (let ((s "abcdef"))
    (values
     (loop for i from 0 to 6
	   collect (string-upcase s :start i))
     s))
  ("ABCDEF" "aBCDEF" "abCDEF" "abcDEF" "abcdEF" "abcdeF" "abcdef")
  "abcdef")

(deftest string-upcase.9
  (let ((s "abcdef"))
    (values
      (loop for i from 0 to 6
	    collect 
	    (string-upcase s :start i :end nil))
      s))
  ("ABCDEF" "aBCDEF" "abCDEF" "abcDEF" "abcdEF" "abcdeF" "abcdef")
  "abcdef")

(deftest string-upcase.10
  (let ((s "abcde"))
    (values
     (loop for i from 0 to 4
	   collect (loop for j from i to 5
			 collect (string-upcase s :start i :end j)))
     s))
  (("abcde" "Abcde" "ABcde" "ABCde" "ABCDe" "ABCDE")
   ("abcde" "aBcde" "aBCde" "aBCDe" "aBCDE")
   ("abcde" "abCde" "abCDe" "abCDE")
   ("abcde" "abcDe" "abcDE")
   ("abcde" "abcdE"))
  "abcde")

(deftest string-upcase.order.1
  (let ((i 0) a b c (s (copy-seq "abcdef")))
    (values
     (string-upcase
      (progn (setf a (incf i)) s)
      :start (progn (setf b (incf i)) 1)
      :end   (progn (setf c (incf i)) 4))
     i a b c))
  "aBCDef" 3 1 2 3)

(deftest string-upcase.order.2
  (let ((i 0) a b c (s (copy-seq "abcdef")))
    (values
     (string-upcase
      (progn (setf a (incf i)) s)
      :end   (progn (setf b (incf i)) 4)
      :start (progn (setf c (incf i)) 1))
     i a b c))
  "aBCDef" 3 1 2 3)

  
;;; Error cases

(deftest string-upcase.error.1
  (classify-error (string-upcase))
  program-error)

(deftest string-upcase.error.2
  (classify-error (string-upcase (copy-seq "abc") :bad t))
  program-error)

(deftest string-upcase.error.3
  (classify-error (string-upcase (copy-seq "abc") :start))
  program-error)

(deftest string-upcase.error.4
  (classify-error (string-upcase (copy-seq "abc") :bad t
				      :allow-other-keys nil))
  program-error)

(deftest string-upcase.error.5
  (classify-error (string-upcase (copy-seq "abc") :end))
  program-error)

(deftest string-upcase.error.6
  (classify-error (string-upcase (copy-seq "abc") 1 2))
  program-error)
