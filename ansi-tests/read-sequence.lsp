;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jan 19 06:55:04 2004
;;;; Contains: Tests of READ-SEQUENCE

(in-package :cl-test)

;;; Read into a string

(deftest read-sequence.string.1
  (let ((s (make-string 5)))
    (with-input-from-string
     (is "abcdefghijk")
     (values
      (read-sequence s is)
      s)))
  5
  "abcde")

(deftest read-sequence.string.2
  (let ((s (make-string 5 :initial-element #\Space)))
    (with-input-from-string
     (is "abc")
     (values
      (read-sequence s is)
      s)))
  3
  "abc  ")

(deftest read-sequence.string.3
  (let ((s (make-string 5 :initial-element #\Space)))
    (with-input-from-string
     (is "abcdefghijk")
     (values
      (read-sequence s is :start 1)
      s)))
  5
  " abcd")

(deftest read-sequence.string.4
  (let ((s (make-string 5 :initial-element #\Space)))
    (with-input-from-string
     (is "abcdefghijk")
     (values
      (read-sequence s is :end 3)
      s)))
  3
  "abc  ")

(deftest read-sequence.string.5
  (let ((s (make-string 5 :initial-element #\Space)))
    (with-input-from-string
     (is "abcdefghijk")
     (values
      (read-sequence s is :start 1 :end 4)
      s)))
  4
  " abc ")

(deftest read-sequence.string.6
  (let ((s (make-string 5 :initial-element #\Space)))
    (with-input-from-string
     (is "abcdefghijk")
     (values
      (read-sequence s is :start 0 :end 0)
      s)))
  0
  "     ")

;;; Read into a base string

(deftest read-sequence.base-string.1
  (let ((s (make-array 5 :element-type 'base-char)))
    (with-input-from-string
     (is "abcdefghijk")
     (values
      (read-sequence s is)
      s)))
  5
  "abcde")

(deftest read-sequence.base-string.2
  (let ((s (make-array 5 :initial-element #\Space
		       :element-type 'base-char)))
    (with-input-from-string
     (is "abc")
     (values
      (read-sequence s is)
      s)))
  3
  "abc  ")

(deftest read-sequence.base-string.3
  (let ((s (make-array 5 :initial-element #\Space
		       :element-type 'base-char)))
    (with-input-from-string
     (is "abcdefghijk")
     (values
      (read-sequence s is :start 1)
      s)))
  5
  " abcd")

(deftest read-sequence.base-string.4
  (let ((s (make-array 5 :initial-element #\Space
		       :element-type 'base-char)))
    (with-input-from-string
     (is "abcdefghijk")
     (values
      (read-sequence s is :end 3)
      s)))
  3
  "abc  ")

(deftest read-sequence.base-string.5
  (let ((s (make-array 5 :initial-element #\Space
		       :element-type 'base-char)))
    (with-input-from-string
     (is "abcdefghijk")
     (values
      (read-sequence s is :start 1 :end 4)
      s)))
  4
  " abc ")

(deftest read-sequence.base-string.6
  (let ((s (make-array 5 :initial-element #\Space
		       :element-type 'base-char)))
    (with-input-from-string
     (is "abcdefghijk")
     (values
      (read-sequence s is :start 0 :end 0)
      s)))
  0
  "     ")

;;; Read into a list

(deftest read-sequence.list.1
  (let ((s (make-list 5)))
    (with-input-from-string
     (is "abcde")
     (values
      (read-sequence s is)
      s)))
  5
  (#\a #\b #\c #\d #\e))

(deftest read-sequence.list.2
  (let ((s (make-list 5)))
    (with-input-from-string
     (is "abc")
     (values
      (read-sequence s is)
      s)))
  3
  (#\a #\b #\c nil nil))

(deftest read-sequence.list.3
  (let ((s (make-list 5)))
    (with-input-from-string
     (is "abcdefghijk")
     (values
      (read-sequence s is :start 1)
      s)))
  5
  (nil #\a #\b #\c #\d))

(deftest read-sequence.list.4
  (let ((s (make-list 5)))
    (with-input-from-string
     (is "abcdefghijk")
     (values
      (read-sequence s is :end 3)
      s)))
  3
  (#\a #\b #\c nil nil))

(deftest read-sequence.list.5
  (let ((s (make-list 5)))
    (with-input-from-string
     (is "abcdefghijk")
     (values
      (read-sequence s is :start 1 :end 4)
      s)))
  4
  (nil #\a #\b #\c nil))

(deftest read-sequence.list.6
  (let ((s (make-list 5)))
    (with-input-from-string
     (is "abcdefghijk")
     (values
      (read-sequence s is :start 0 :end 0)
      s)))
  0
  (nil nil nil nil nil))

;;; Read into a vector

(deftest read-sequence.vector.1
  (let ((s (vector nil nil nil nil nil)))
    (with-input-from-string
     (is "abcde")
     (values
      (read-sequence s is)
      s)))
  5
  #(#\a #\b #\c #\d #\e))

(deftest read-sequence.vector.2
  (let ((s (vector nil nil nil nil nil)))
    (with-input-from-string
     (is "abc")
     (values
      (read-sequence s is)
      s)))
  3
  #(#\a #\b #\c nil nil))

(deftest read-sequence.vector.3
  (let ((s (vector nil nil nil nil nil)))
    (with-input-from-string
     (is "abcdefghijk")
     (values
      (read-sequence s is :start 1)
      s)))
  5
  #(nil #\a #\b #\c #\d))

(deftest read-sequence.vector.4
  (let ((s (vector nil nil nil nil nil)))
    (with-input-from-string
     (is "abcdefghijk")
     (values
      (read-sequence s is :end 3)
      s)))
  3
  #(#\a #\b #\c nil nil))

(deftest read-sequence.vector.5
  (let ((s (vector nil nil nil nil nil)))
    (with-input-from-string
     (is "abcdefghijk")
     (values
      (read-sequence s is :start 1 :end 4)
      s)))
  4
  #(nil #\a #\b #\c nil))

(deftest read-sequence.vector.6
  (let ((s (vector nil nil nil nil nil)))
    (with-input-from-string
     (is "abcdefghijk")
     (values
      (read-sequence s is :start 0 :end 0)
      s)))
  0
  #(nil nil nil nil nil))

;;; Read into a bit vector

(defmacro def-read-sequence-bv-test (name init args &rest expected)
  `(deftest ,name
     ;; Create output file
     (let ((os (open "temp.dat" :direction :output
		     :element-type '(unsigned-byte 8)
		     :if-exists :supersede)))
       (loop for i in '(0 1 1 0 0 1 1 0 1 0 1 1 1 0)
	     do (write-byte i os))
       (close os)
       (let ((is (open "temp.dat" :direction :input
		       :element-type '(unsigned-byte 8)))
	     (bv (copy-seq ,init)))
	 (values
	  (read-sequence bv is ,@args)
	  bv)))
     ,@expected))
     
(def-read-sequence-bv-test read-sequence.bv.1 #*00000000000000 ()
  14 #*01100110101110)
  
(def-read-sequence-bv-test read-sequence.bv.2 #*00000000000000 (:start 0)
  14 #*01100110101110)
  
(def-read-sequence-bv-test read-sequence.bv.3 #*00000000000000 (:end 14)
  14 #*01100110101110)
  
(def-read-sequence-bv-test read-sequence.bv.4 #*00000000000000 (:end nil)
  14 #*01100110101110)
  
(def-read-sequence-bv-test read-sequence.bv.5 #*00000000000000 (:start 2)
  14 #*00011001101011)
  
(def-read-sequence-bv-test read-sequence.bv.6 #*00000000000000
  (:start 2 :end 13)
  13 #*00011001101010)

(def-read-sequence-bv-test read-sequence.bv.7 #*00000000000000 (:end 6)
  6 #*01100100000000)

;;; Error cases

(deftest read-sequence.error.1
  (signals-error (read-sequence) program-error)
  t)

(deftest read-sequence.error.2
  (signals-error (read-sequence (make-string 10)) program-error)
  t)

(deftest read-sequence.error.3
  (signals-error
   (read-sequence (make-string 5) (make-string-input-stream "abc") :start)
   program-error)
  t)

(deftest read-sequence.error.4
  (signals-error
   (read-sequence (make-string 5) (make-string-input-stream "abc") :foo 1)
   program-error)
  t)

(deftest read-sequence.error.5
  (signals-error
   (read-sequence (make-string 5) (make-string-input-stream "abc")
		  :allow-other-keys nil :bar 2)
   program-error)
  t)

(deftest read-sequence.error.6
  (signals-error
   (read-sequence 'a (make-string-input-stream "abc"))
   type-error)
  t)

(deftest read-sequence.error.7
  (signals-error
   (read-sequence (cons 'a 'b) (make-string-input-stream "abc"))
   type-error)
  t)

(deftest read-sequence.error.8
  (signals-error
   (read-sequence (make-string 3) (make-string-input-stream "abc")
		  :start -1)

   type-error)
  t)

(deftest read-sequence.error.9
  (signals-error
   (read-sequence (make-string 3) (make-string-input-stream "abc")
		  :start 'a)

   type-error)
  t)

(deftest read-sequence.error.10
  (signals-error
   (read-sequence (make-string 3) (make-string-input-stream "abc")
		  :end -1)
   type-error)
  t)

(deftest read-sequence.error.11
  (signals-error
   (read-sequence (make-string 3) (make-string-input-stream "abc")
		  :end 'b)
   type-error)
  t)



