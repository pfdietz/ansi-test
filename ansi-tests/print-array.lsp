;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Apr 22 22:38:11 2004
;;;; Contains: Tests of printing of arrays (other than vectors)

(compile-and-load "printer-aux.lsp")

(in-package :cl-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zero dimensional arrays

(deftest print.array.0.1
  (let ((a (make-array nil :initial-element 0)))
    (with-standard-io-syntax
     (write-to-string a :readably nil :array t)))
  "#0A0")

(deftest print.array.0.2
  (with-standard-io-syntax
   (let ((a (make-array nil :initial-element '|A|))
	 (*package* (find-package "CL-TEST")))
     (write-to-string a :readably nil :array t)))
  "#0AA")

(deftest print.array.0.3
  (let ((a (make-array nil :initial-element 0)))
    (subseq (write-to-string a :readably nil :array nil) 0 2))
  "#<")

(deftest print.array.0.4
   (let ((a (make-array nil :initial-element 0 :adjustable t)))
    (with-standard-io-syntax
     (write-to-string a :readably nil :array t)))
  "#0A0")

(deftest print.array.0.5
   (let* ((a (make-array nil :initial-element 0 :adjustable t))
	  (b (make-array nil :displaced-to a :displaced-index-offset 0)))
    (with-standard-io-syntax
     (write-to-string b :readably nil :array t)))
  "#0A0")

(deftest print.array.0.6
  (let ((a (make-array nil :initial-element 0
		       :element-type '(integer 0 2))))
    (with-standard-io-syntax
     (write-to-string a :readably nil :array t)))
  "#0A0")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Two-d arrays
(deftest print.array.2.1
  (let ((a (make-array '(1 1) :initial-contents '((1)))))
    (with-standard-io-syntax
     (write-to-string a :readably nil :array t)))
  "#2A((1))")

(deftest print.array.2.2
  (let ((a (make-array '(2 3) :initial-contents '((1 3 8)(2 6 10)))))
    (with-standard-io-syntax
     (write-to-string a :readably nil :array t)))
  "#2A((1 3 8) (2 6 10))")

(deftest print.array.2.3
  (let ((a (make-array '(0 1))))
    (with-standard-io-syntax
     (write-to-string a :readably nil :array t)))
  "#2A()")

(deftest print.array.2.4
  (let ((a (make-array '(1 0))))
    (with-standard-io-syntax
     (write-to-string a :readably nil :array t)))
  "#2A(())")

(deftest print.array.2.5
  (let ((a (make-array '(0 0))))
    (with-standard-io-syntax
     (write-to-string a :readably nil :array t)))
  "#2A()")

(deftest print.array.2.6
  (let ((a (make-array '(10 0))))
    (with-standard-io-syntax
     (write-to-string a :readably nil :array t)))
  "#2A(() () () () () () () () () ())")

(deftest print.array.2.7
  (let* ((a (make-array '(3 3) :initial-contents '((1 3 8) (2 67 121) (65 432 6))))
	 (b (make-array '(3 3) :displaced-to a
			:displaced-index-offset 0)))
    (with-standard-io-syntax
     (write-to-string b :readably nil :array t)))
  "#2A((1 3 8) (2 67 121) (65 432 6))")

(deftest print.array.2.8
  (let* ((a (make-array '(3 3) :initial-contents '((1 3 8) (2 67 121) (65 432 6))))
	 (b (make-array '(2 3) :displaced-to a
			:displaced-index-offset 0)))
    (with-standard-io-syntax
     (write-to-string b :readably nil :array t)))
  "#2A((1 3 8) (2 67 121))")

(deftest print.array.2.9
  (let* ((a (make-array '(3 3) :initial-contents '((1 3 8) (2 67 121) (65 432 6))))
	 (b (make-array '(2 2) :displaced-to a
			:displaced-index-offset 4)))
    (with-standard-io-syntax
     (write-to-string b :readably nil :array t)))
  "#2A((67 121) (65 432))")

(deftest print.array.2.10
  (let* ((a (make-array '(3 3) :initial-contents '((1 3 8) (2 67 121) (65 432 6))))
	 (b (make-array '(2 2) :displaced-to a
			:displaced-index-offset 4
			:adjustable t)))
    (with-standard-io-syntax
     (write-to-string b :readably nil :array t)))
  "#2A((67 121) (65 432))")

(deftest print.array.2.11
  (let* ((a (make-array '(3 4)
			:initial-contents '((7 8 9 10) (65 12 42 -1) (:|W| :|X| :|Y| :|Z| ))
			:adjustable t)))
    (with-standard-io-syntax
     (write-to-string a :readably nil :array t)))
  "#2A((7 8 9 10) (65 12 42 -1) (:W :X :Y :Z))")

(deftest print.array.2.12
  (let ((desired-result "#2A((0 1 1) (1 1 0))"))
    (loop for i from 2 to 64
	  for a = (make-array '(2 3) :element-type `(unsigned-byte ,i)
			      :initial-contents '((0 1 1) (1 1 0)))
	  for result = (with-standard-io-syntax
			(write-to-string a :readably nil :array t))
	  unless (string= desired-result result)
	  collect (list i a result)))
  nil)

(deftest print.array.2.13
  (let ((desired-result "#2A((0 -1 -1) (-1 -1 0))"))
    (loop for i from 1 to 64
	  for a = (make-array '(2 3) :element-type `(signed-byte ,i)
			      :initial-contents '((0 -1 -1) (-1 -1 0)))
	  for result = (with-standard-io-syntax
			(write-to-string a :readably nil :array t))
	  unless (string= desired-result result)
	  collect (list i a result)))
  nil)

(deftest print.array.2.14
  (let ((desired-result "#2A((0 1 1) (1 1 0))"))
    (loop for i from 2 to 64
	  for a = (make-array '(2 3) :element-type `(unsigned-byte ,i)
			      :adjustable t
			      :initial-contents '((0 1 1) (1 1 0)))
	  for result = (with-standard-io-syntax
			(write-to-string a :readably nil :array t))
	  unless (string= desired-result result)
	  collect (list i a result)))
  nil)

(deftest print.array.2.15
  (let ((desired-result "#2A((0 -1 -1) (-1 -1 0))"))
    (loop for i from 1 to 64
	  for a = (make-array '(2 3) :element-type `(signed-byte ,i)
			      :adjustable t
			      :initial-contents '((0 -1 -1) (-1 -1 0)))
	  for result = (with-standard-io-syntax
			(write-to-string a :readably nil :array t))
	  unless (string= desired-result result)
	  collect (list i a result)))
  nil)

(deftest print.array.2.16
  (let ((desired-result "#2A((1 1) (1 0))"))
    (loop for i from 2 to 64
	  for type = `(unsigned-byte ,i)
	  for a = (make-array '(2 3) :element-type type
			      :adjustable t
			      :initial-contents '((0 1 1) (1 1 0)))
	  for b = (make-array '(2 2) :displaced-to a :displaced-index-offset 2
			      :element-type type)
	  for result = (with-standard-io-syntax
			(write-to-string b :readably nil :array t))
	  unless (string= desired-result result)
	  collect (list i b result)))
  nil)

(deftest print.array.2.17
  (let ((desired-result "#2A((1 -1) (-2 0))"))
    (loop for i from 2 to 64
	  for type = `(signed-byte ,i)
	  for a = (make-array '(2 3) :element-type type
			      :adjustable t
			      :initial-contents '((0 1 1) (-1 -2 0)))
	  for b = (make-array '(2 2) :displaced-to a :displaced-index-offset 2
			      :element-type type)
	  for result = (with-standard-io-syntax
			(write-to-string b :readably nil :array t))
	  unless (string= desired-result result)
	  collect (list i b result)))
  nil)

(deftest print.array.2.20
  (let* ((a (make-array '(9) :initial-contents '(1 3 8 2 67 121 65 432 6)))
	 (b (make-array '(2 2) :displaced-to a
			:displaced-index-offset 1)))
    (with-standard-io-syntax
     (write-to-string b :readably nil :array t)))
  "#2A((3 8) (2 67))")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Three D arrays

(deftest print.array.3.1
  (let* ((a (make-array '(1 2 3) :initial-contents '(((:|A| :|B| :|C|) (:|D| :|E| :|F|)))))
	 (b (make-array '(3 2 1) :displaced-to a
			:displaced-index-offset 0)))
    (with-standard-io-syntax
     (values
      (write-to-string a :readably nil :array t)
      (write-to-string b :readably nil :array t))))
  "#3A(((:A :B :C) (:D :E :F)))"
  "#3A(((:A) (:B)) ((:C) (:D)) ((:E) (:F)))")


;;; Multidimensional arrays

(deftest print.array.multi-dim.1
  (with-standard-io-syntax
   (loop for d in '(4 5 6 7 8 9 10 12 16 20 30 40 100 200 400 600 800 1023)
	 for dims = (make-list d :initial-element 1)
	 for a = (make-array dims :initial-element 0)
	 for result = (with-standard-io-syntax
		       (write-to-string a :readably nil :array t))
	 for expected-result =
	 (concatenate 'string
		      (format nil "#~DA" d)
		      (make-string d :initial-element #\()
		      "0"
		      (make-string d :initial-element #\)))
	 unless (string= result expected-result)
	 collect (list d result expected-result)))
  nil)

(deftest print.array.multi-dim.2
  (with-standard-io-syntax
   (loop for d = (+ 4 (random 1020))
	 for p = (random d)
	 for dims = (let ((list (make-list d :initial-element 1)))
		      (setf (elt list p) 0)
		      list)
	 for a = (make-array dims :initial-element 0)
	 for result = (with-standard-io-syntax
		       (write-to-string a :readably nil :array t))
	 for expected-result =
	 (concatenate 'string
		      (format nil "#~DA" d)
		      (make-string (1+ p) :initial-element #\()
		      (make-string (1+ p) :initial-element #\)))
	 repeat 50
	 unless (string= result expected-result)
	 collect (list d result expected-result)))
  nil)

