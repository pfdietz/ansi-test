;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jan 17 21:02:13 2004
;;;; Contains: Tests of PEEK-CHAR

(in-package :cl-test)

(deftest peek-char.1
  (with-input-from-string
   (*standard-input* "abc")
   (values
    (peek-char)
    (read-char)
    (read-char)
    (peek-char)
    (read-char)))
  #\a #\a #\b #\c #\c)

(deftest peek-char.2
  (with-input-from-string
   (*standard-input* "   ab")
   (values
    (peek-char)
    (read-char)
    (peek-char t)
    (read-char)
    (peek-char t)
    (read-char)))
  #\Space #\Space #\a #\a #\b #\b)

(deftest peek-char.3
  (with-input-from-string
   (*standard-input* (concatenate 'string
				  (string #\Newline)
				  (string #\Newline)
				  "  "
				  (string #\Newline)
				  "ab"))
   (values
    (peek-char)
    (read-char)
    (peek-char t)
    (read-char)
    (peek-char t)
    (read-char)))
  #\Newline #\Newline #\a #\a #\b #\b)

(when (name-char "Linefeed")
  (deftest peek-char.4
    (with-input-from-string
     (*standard-input* (concatenate 'string
				    (string (name-char "Linefeed"))
				    (string (name-char "Linefeed"))
				    "abc"))
     (values
      (peek-char)
      (read-char)
      (peek-char t)
      (read-char)))
    #.(name-char "Linefeed")
    #.(name-char "Linefeed")
    #\a #\a))

(when (name-char "Page")
  (deftest peek-char.5
    (with-input-from-string
     (*standard-input* (concatenate 'string
				    (string (name-char "Page"))
				    (string (name-char "Page"))
				    "abc"))
     (values
      (peek-char)
      (read-char)
      (peek-char t)
      (read-char)))
    #.(name-char "Page")
    #.(name-char "Page")
    #\a #\a))

(when (name-char "Tab")
  (deftest peek-char.6
    (with-input-from-string
     (*standard-input* (concatenate 'string
				    (string (name-char "Tab"))
				    (string (name-char "Tab"))
				    "abc"))
     (values
      (peek-char)
      (read-char)
      (peek-char t)
      (read-char)))
    #.(name-char "Tab")
    #.(name-char "Tab")
    #\a #\a))

(when (name-char "Return")
  (deftest peek-char.7
    (with-input-from-string
     (*standard-input* (concatenate 'string
				    (string (name-char "Return"))
				    (string (name-char "Return"))
				    "abc"))
     (values
      (peek-char)
      (read-char)
      (peek-char t)
      (read-char)))
    #.(name-char "Return")
    #.(name-char "Return")
    #\a #\a))

(deftest peek-char.8
  (with-input-from-string
   (s "a bcd")
   (values
    (peek-char nil s)
    (read-char s)
    (peek-char t s)
    (read-char s)
    (peek-char t s)
    (read-char s)))
  #\a #\a #\b #\b #\c #\c)

(deftest peek-char.9
  (with-input-from-string
   (*standard-input* " a bCcde")
   (values
    (peek-char #\c)
    (read-char)
    (read-char)))
  #\c #\c #\d)

(deftest peek-char.10
  (with-input-from-string
   (*standard-input* "  ; foo")
   (values
    (peek-char t)
    (read-char)))
  #\; #\;)

(deftest peek-char.11
  (with-input-from-string
   (s "")
   (peek-char nil s nil))
  nil)

(deftest peek-char.12
  (with-input-from-string
   (s "")
   (peek-char nil s nil 'foo))
  foo)

(deftest peek-char.13
  (with-input-from-string
   (s "   ")
   (peek-char t s nil))
  nil)

(deftest peek-char.14
  (with-input-from-string
   (s "   ")
   (peek-char t s nil 'foo))
  foo)

(deftest peek-char.15
  (with-input-from-string
   (s "ab c d")
   (peek-char #\z s nil))
  nil)

(deftest peek-char.16
  (with-input-from-string
   (s "ab c d")
   (peek-char #\z s nil 'foo))
  foo)


;;; Error tests

(deftest peek-char.error.1
  (signals-error
   (with-input-from-string
    (s "abc")
    (peek-char s nil nil nil nil 'nonsense))
   program-error)
  t)


(deftest peek-char.error.2
  (signals-error
   (with-input-from-string
    (*standard-input* "")
    (peek-char))
   end-of-file)
  t)

(deftest peek-char.error.3
  (signals-error
   (with-input-from-string
    (s "")
    (peek-char nil s))
   end-of-file)
  t)

(deftest peek-char.error.4
  (signals-error
   (with-input-from-string
    (s " ")
    (peek-char t s))
   end-of-file)
  t)

(deftest peek-char.error.5
  (signals-error
   (with-input-from-string
    (s "abcd")
    (peek-char #\z s))
   end-of-file)
  t)

(deftest peek-char.error.6
  (signals-error
   (with-input-from-string
    (s "")
    (peek-char nil s nil nil t))
   end-of-file)
  t)
