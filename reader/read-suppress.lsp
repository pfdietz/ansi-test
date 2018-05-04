;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jan 15 13:55:27 2005
;;;; Contains: Tests of reading with *READ-SUPPRESS* bound to true

(in-package :cl-test)



(defmacro def-read-suppress-test (name string)
  `(def-syntax-test ,name
     (let ((*read-suppress* t))
       (read-from-string ,string))
     nil ,(length string)))

(def-read-suppress-test read-suppress.1  "NONEXISTENT-PACKAGE::FOO")
(def-read-suppress-test read-suppress.2  ":")
(def-read-suppress-test read-suppress.3 "::")
(def-read-suppress-test read-suppress.4 ":::")
(def-read-suppress-test read-suppress.5 "123.45")
;; (def-read-suppress-test read-suppress.6 ".")
(def-read-suppress-test read-suppress.7 "..")
(def-read-suppress-test read-suppress.8 "...")
(def-read-suppress-test read-suppress.9 "(1 2)")
(def-read-suppress-test read-suppress.10 "(1 . 2)")
(def-read-suppress-test read-suppress.11 "(1 .. 2 . 3)")
(def-read-suppress-test read-suppress.12 "(...)")

(defparameter *non-macro-chars*
  (coerce "1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-=+_~!@$%^&*{}[]<>/?."
          'simple-base-string))

(declaim (type simple-base-string *non-macro-chars*))

(defmacro def-random-suppress-test (name &key
                                         (chars '*non-macro-chars*)
                                         (reps 1000)
                                         (maxlen 8)
                                         (count 10)
                                         (prefix "")
                                         (suffix ""))
  `(def-syntax-test ,name
     (let* ((chars ,chars)
            (prefix ,prefix)
            (suffix ,suffix)
            (*read-suppress* t)
            (count 0)
            (maxlen ,maxlen)
            (reps ,reps)
            (maxcount ,count))
       (loop for n = (1+ (random maxlen))
             for s = (concatenate 'string
                                  prefix
                                  (loop repeat n
                                        collect (random-from-seq chars))
                                  suffix)
             for vals = (multiple-value-list
                         (handler-case (read-from-string s)
                                       (reader-error (rc) rc)))
             repeat reps
             unless (equal vals (list nil (length s)))
             collect (progn (when (> (incf count) maxcount)
                              (loop-finish))
                            (list n s vals))))
     nil))

(def-random-suppress-test read-suppress.13)
(def-random-suppress-test read-suppress.14 :prefix "(" :suffix ")")
(def-random-suppress-test read-suppress.15 :prefix "#(" :suffix ")")
(def-random-suppress-test read-suppress.16 :chars "0123456789.eEfFsSdDlL+-")

;; Undefined macro dispatch characters should not signal an error
(def-read-suppress-test read-suppress.17 "#garbage")

(def-read-suppress-test read-suppress.sharp-slash.1 "#\\boguscharname")
(def-read-suppress-test read-suppress.sharp-slash.2 "#\\:x")
(def-read-suppress-test read-suppress.sharp-slash.3 "#\\::::")
(def-read-suppress-test read-suppress.sharp-slash.4 "#\\123")
(def-read-suppress-test read-suppress.sharp-slash.5 "#0\\ ")
(def-read-suppress-test read-suppress.sharp-slash.6 "#100000000\\Space")

(def-read-suppress-test read-suppress.sharp-quote.1 "#'foo")
(def-read-suppress-test read-suppress.sharp-quote.2 "#'1")
(def-read-suppress-test read-suppress.sharp-quote.3 "#'(setf bar)")
(def-read-suppress-test read-suppress.sharp-quote.5 "#'.")
(def-read-suppress-test read-suppress.sharp-quote.6 "#'1.2.3")
(def-read-suppress-test read-suppress.sharp-quote.7 "#0'F")
(def-read-suppress-test read-suppress.sharp-quote.8 "#1000000'F")

(def-read-suppress-test read-suppress.sharp-left-paren.1 "#()")
(def-read-suppress-test read-suppress.sharp-left-paren.2 "#(A)")
(def-read-suppress-test read-suppress.sharp-left-paren.3 "#(A B)")
(def-read-suppress-test read-suppress.sharp-left-paren.4 "#0()")
(def-read-suppress-test read-suppress.sharp-left-paren.5 "#0(A)")
(def-read-suppress-test read-suppress.sharp-left-paren.6 "#1(A)")
(def-read-suppress-test read-suppress.sharp-left-paren.7 "#1(A B C D E)")
(def-read-suppress-test read-suppress.sharp-left-paren.8 "#4(A B C D E)")
(def-read-suppress-test read-suppress.sharp-left-paren.9 "#10(A B C D E)")
(def-read-suppress-test read-suppress.sharp-left-paren.10 "#100()")
(def-read-suppress-test read-suppress.sharp-left-paren.11 "#10000000000000()")
(def-read-suppress-test read-suppress.sharp-left-paren.12 "#10000000000000(A)")

(def-read-suppress-test read-suppress.sharp-asterisk.1 "#*")
(def-read-suppress-test read-suppress.sharp-asterisk.2 "#0*")
(def-read-suppress-test read-suppress.sharp-asterisk.3 "#*1")
(def-read-suppress-test read-suppress.sharp-asterisk.4 "#*0111001")
(def-read-suppress-test read-suppress.sharp-asterisk.5 "#*73298723497132")
(def-read-suppress-test read-suppress.sharp-asterisk.6
  "#*abcdefghijklmnopqrstuvwxyz")
(def-read-suppress-test read-suppress.sharp-asterisk.7
  "#*ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(def-read-suppress-test read-suppress.sharp-asterisk.8 "#*:")
(def-read-suppress-test read-suppress.sharp-asterisk.9 "#*::::")
(def-read-suppress-test read-suppress.sharp-asterisk.10 "#1*")
(def-read-suppress-test read-suppress.sharp-asterisk.11 "#10000*")
(def-read-suppress-test read-suppress.sharp-asterisk.12 "#10000000000000*")
(def-read-suppress-test read-suppress.sharp-asterisk.13 "#4*001101001")
(def-read-suppress-test read-suppress.sharp-asterisk.14 "#2*")

(def-read-suppress-test read-suppress.sharp-colon.1 "#:1")
(def-read-suppress-test read-suppress.sharp-colon.2 "#:foo")
(def-read-suppress-test read-suppress.sharp-colon.3 "#0:1/2")
(def-read-suppress-test read-suppress.sharp-colon.4 "#10:-2")
(def-read-suppress-test read-suppress.sharp-colon.5 "#100000000000:x")
(def-read-suppress-test read-suppress.sharp-colon.6 "#3:foo")
(def-read-suppress-test read-suppress.sharp-colon.7 "#::")
(def-read-suppress-test read-suppress.sharp-colon.8 "#:123")
(def-read-suppress-test read-suppress.sharp-colon.9 "#:.")

(def-read-suppress-test read-suppress.sharp-dot.1 "#.1")
(def-read-suppress-test read-suppress.sharp-dot.2 "#.#:foo")
(def-read-suppress-test read-suppress.sharp-dot.3 "#.(throw 'foo nil)")
(def-read-suppress-test read-suppress.sharp-dot.4 "#0.1")
(def-read-suppress-test read-suppress.sharp-dot.5 "#10.1")
(def-read-suppress-test read-suppress.sharp-dot.6 "#1000000000000000.1")

(def-read-suppress-test read-suppress.sharp-b.1 "#b0")
(def-read-suppress-test read-suppress.sharp-b.2 "#B1")
(def-read-suppress-test read-suppress.sharp-b.3 "#BX")
(def-read-suppress-test read-suppress.sharp-b.4 "#b.")
(def-read-suppress-test read-suppress.sharp-b.5 "#0b0")
(def-read-suppress-test read-suppress.sharp-b.6 "#1B1")
(def-read-suppress-test read-suppress.sharp-b.7 "#100b010")
(def-read-suppress-test read-suppress.sharp-b.8 "#1000000000000b010")
(def-read-suppress-test read-suppress.sharp-b.9 "#B101/100")
(def-read-suppress-test read-suppress.sharp-b.10 "#b101/100/11")

(def-read-suppress-test read-suppress.sharp-o.1 "#o0")
(def-read-suppress-test read-suppress.sharp-o.2 "#O1")
(def-read-suppress-test read-suppress.sharp-o.3 "#OX")
(def-read-suppress-test read-suppress.sharp-o.4 "#o.")
(def-read-suppress-test read-suppress.sharp-o.5 "#od6")
(def-read-suppress-test read-suppress.sharp-o.6 "#1O9")
(def-read-suppress-test read-suppress.sharp-o.7 "#100O010")
(def-read-suppress-test read-suppress.sharp-o.8 "#1000000000000o27423")
(def-read-suppress-test read-suppress.sharp-o.9 "#O123/457")
(def-read-suppress-test read-suppress.sharp-o.10 "#o12/17/21")

(def-read-suppress-test read-suppress.sharp-c.1 "#c(0 0)")
(def-read-suppress-test read-suppress.sharp-c.2 "#C(1.0 1.0)")
(def-read-suppress-test read-suppress.sharp-c.3 "#cFOO")
(def-read-suppress-test read-suppress.sharp-c.4 "#c1")
(def-read-suppress-test read-suppress.sharp-c.5 "#C(1 2 3)")
(def-read-suppress-test read-suppress.sharp-c.6 "#c.")
(def-read-suppress-test read-suppress.sharp-c.7 "#c()")
(def-read-suppress-test read-suppress.sharp-c.8 "#c(1)")
(def-read-suppress-test read-suppress.sharp-c.9 "#C(1 . 2)")
(def-read-suppress-test read-suppress.sharp-c.10 "#c(1 2 3)")
(def-read-suppress-test read-suppress.sharp-c.11 "#0c(1 2)")
(def-read-suppress-test read-suppress.sharp-c.12 "#1C(1 2)")
(def-read-suppress-test read-suppress.sharp-c.13 "#10c(1 2)")
(def-read-suppress-test read-suppress.sharp-c.14 "#123456789c(1 2)")
(def-read-suppress-test read-suppress.sharp-c.15 "#c(..)")

(def-read-suppress-test read-suppress.sharp-x.1 "#x0")
(def-read-suppress-test read-suppress.sharp-x.2 "#X1")
(def-read-suppress-test read-suppress.sharp-x.3 "#XX")
(def-read-suppress-test read-suppress.sharp-x.4 "#x.")
(def-read-suppress-test read-suppress.sharp-x.5 "#xy6")
(def-read-suppress-test read-suppress.sharp-x.6 "#1X9")
(def-read-suppress-test read-suppress.sharp-x.7 "#100X010")
(def-read-suppress-test read-suppress.sharp-x.8 "#1000000000000x2af23")
(def-read-suppress-test read-suppress.sharp-x.9 "#X123/DE7")
(def-read-suppress-test read-suppress.sharp-x.10 "#x12/17/21")

(def-read-suppress-test read-suppress.sharp-r.1 "#2r1101")
(def-read-suppress-test read-suppress.sharp-r.2 "#10R9871")
(def-read-suppress-test read-suppress.sharp-r.3 "#36r721zwoqnASLDKJA22")
(def-read-suppress-test read-suppress.sharp-r.4 "#r.")
(def-read-suppress-test read-suppress.sharp-r.5 "#2r379ze")
(def-read-suppress-test read-suppress.sharp-r.6 "#0r0")
(def-read-suppress-test read-suppress.sharp-r.7 "#1r0")
(def-read-suppress-test read-suppress.sharp-r.8 "#100r0A")
(def-read-suppress-test read-suppress.sharp-r.9 "#1000000000000r0A")
(def-read-suppress-test read-suppress.sharp-r.10 "#2r!@#$%^&*_-+={}[]:<>.?/")

(def-read-suppress-test read-suppress.sharp-a.1 "#a()")
(def-read-suppress-test read-suppress.sharp-a.2 "#2a((a)(b c))")
(def-read-suppress-test read-suppress.sharp-a.3 "#a1")
(def-read-suppress-test read-suppress.sharp-a.4 "#1a1")
(def-read-suppress-test read-suppress.sharp-a.5 "#10a(a b c)")
(def-read-suppress-test read-suppress.sharp-a.6 "#100a(a b c)")
(def-read-suppress-test read-suppress.sharp-a.7 "#10000000000000a(a b c)")
(def-read-suppress-test read-suppress.sharp-a.8 "#a..")
(def-read-suppress-test read-suppress.sharp-a.9 "#a(...)")

(def-read-suppress-test read-suppress.sharp-s.1 "#s()")
(def-read-suppress-test read-suppress.sharp-s.2 "#S(invalid-sname)")
(def-read-suppress-test read-suppress.sharp-s.3 "#s(..)")
(def-read-suppress-test read-suppress.sharp-s.4 "#S(foo bar)")
(def-read-suppress-test read-suppress.sharp-s.5 "#0s()")
(def-read-suppress-test read-suppress.sharp-s.6 "#1S()")
(def-read-suppress-test read-suppress.sharp-s.7 "#10s()")
(def-read-suppress-test read-suppress.sharp-s.8 "#271S()")
(def-read-suppress-test read-suppress.sharp-s.9 "#712897459812s()")

(def-read-suppress-test read-suppress.sharp-p.1 "#p\"\"")
(def-read-suppress-test read-suppress.sharp-p.2 "#P123")
(def-read-suppress-test read-suppress.sharp-p.3 "#p1/3")
(def-read-suppress-test read-suppress.sharp-p.4 "#0P\"\"")
(def-read-suppress-test read-suppress.sharp-p.5 "#1p\"\"")
(def-read-suppress-test read-suppress.sharp-p.6 "#100P\"\"")
(def-read-suppress-test read-suppress.sharp-p.7 "#1234567890p\"\"")

(def-read-suppress-test read-suppress.sharp-equal.1 "#=nil")
(def-read-suppress-test read-suppress.sharp-equal.2 "#1=nil")
(def-read-suppress-test read-suppress.sharp-equal.3 "#100=nil")
(def-read-suppress-test read-suppress.sharp-equal.4 "(#1=nil #1=nil)")

(def-read-suppress-test read-suppress.sharp-sharp.1 "##")
(def-read-suppress-test read-suppress.sharp-sharp.2 "#1#")
(def-read-suppress-test read-suppress.sharp-sharp.3 "#100#")
(def-read-suppress-test read-suppress.sharp-sharp.4 "#123456789#")

;;; Error cases

(def-syntax-test read-suppress.error.1
  (signals-error (let ((*read-suppress* t)) (read-from-string "')"))
                 reader-error)
  t)

(def-syntax-test read-suppress.error.2
  (signals-error (let ((*read-suppress* t)) (read-from-string "#<"))
                 reader-error)
  t)

(def-syntax-test read-suppress.error.3
  (signals-error (let ((*read-suppress* t)) (read-from-string "# "))
                 reader-error)
  t)

(def-syntax-test read-suppress.error.4
  (signals-error (let ((*read-suppress* t)) (read-from-string "#)"))
                 reader-error)
  t)
