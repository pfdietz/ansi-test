;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Sep  7 10:24:13 2003
;;;; Contains: Tests of PARSE-INTEGER

(in-package :cl-test)

(deftest parse-integer.error.1
  (classify-error (parse-integer))
  program-error)

(deftest parse-integer.error.2
  (classify-error (parse-integer "123" :bogus))
  program-error)

(deftest parse-integer.error.3
  (classify-error (parse-integer "123" :bogus 'foo))
  program-error)

;;;

(deftest parse-integer.1
  (parse-integer "123")
  123 3)

(deftest parse-integer.2
  (parse-integer " 123")
  123 4)

(deftest parse-integer.3
  (parse-integer "    12345678901234567890   ")
  12345678901234567890 24)

(deftest parse-integer.4
  (parse-integer (concatenate 'string (string #\Newline) "17"))
  17 4)

(deftest parse-integer.5
  (let ((c (name-char "Tab")))
    (if c
	(parse-integer (concatenate 'string (string c) "6381" (string c)))
      (values 6381 6)))
  6381 6)

(deftest parse-integer.6
  (let ((c (name-char "Linefeed")))
    (if c
	(parse-integer (concatenate 'string (string c) "-123712" (string c)))
      (values -123712 9)))
  -123712 9)

(deftest parse-integer.7
  (let ((c (name-char "Page")))
    (if c
	(parse-integer (concatenate 'string (string c) "0" (string c)))
      (values 0 3)))
  0 3)

(deftest parse-integer.8
  (let ((c (name-char "Return")))
    (if c
	(parse-integer (concatenate 'string (string c) "999" (string c)))
      (values 999 5)))
  999 5)

(deftest parse-integer.9
  (parse-integer "-0")
  0 2)

(deftest parse-integer.10
  (parse-integer "+0")
  0 2)

(deftest parse-integer.11
  (parse-integer "-00")
  0 3)

(deftest parse-integer.12
  (parse-integer "+000")
  0 4)

(deftest parse-integer.13
  (parse-integer "00010")
  10 5)

(deftest parse-integer.14
  (parse-integer "10110" :radix 2)
  22 5)

(deftest parse-integer.15
  (parse-integer "1021" :radix 3)
  34 4)
