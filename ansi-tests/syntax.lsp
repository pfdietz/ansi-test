;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jan  2 08:12:51 2005
;;;; Contains: Tests of standard syntax

(in-package :cl-test)

;;; Test that 
(def-syntax-test syntax.whitespace.1
  ;; Check that various standard or semistandard characters are whitespace[2]
  (let ((names '("Tab" "Newline" "Linefeed" "Space" "Return" "Page")))
    (loop for name in names
	  for c = (name-char name)
	  nconc
	  (when c
	    (let* ((s (concatenate 'string (string c) "123"))
		   (val (read-from-string s)))
	      (unless (eql val 123)
		(list (list name c s val)))))))
  nil)

(def-syntax-test syntax.constituent.1
  ;; Tests of various characters that they are constituent characters,
  ;; and parse to symbols
  (let ((chars (concatenate
		'string
		"!$%&*<=>?@[]^_-{}+/"
		"abcdefghijklmnopqrstuvwxyz"
		"ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
    (loop for c across chars
	  for s = (string c)
	  for sym = (read-from-string s)
	  unless (string= (symbol-name sym) (string-upcase s))
	  collect (list c sym)))
  nil)

;;; Backspace is an invalid constituent character

(def-syntax-test syntax.backspace.invalid
  (let ((c (name-char "Backspace")))
    (if (not c) t
      (eval `(signals-error (read-from-string (string ,c)) reader-error))))
  t)

;;; Rubout is an invalid constituent character

(def-syntax-test syntax.rubout.invalid
  (let ((c (name-char "Rubout")))
    (if (not c) t
      (eval `(signals-error (read-from-string (string ,c)) reader-error))))
  t)

;;; Digits are alphabetic if >= the read base

(def-syntax-test syntax.digits.alphabetic.1
  (loop for base from 2 to 9
	nconc
	(let ((*read-base* base))
	  (loop for digit-val from base to 9
		for c = (elt "0123456789" digit-val)
		for s = (string c)
		for val = (read-from-string s)
		unless (and (symbolp val)
			    (string= s (symbol-name val)))
		collect (list base digit-val c s val))))
  nil)

;;; Reading escaped characters

(def-syntax-test syntax.escaped.1
  (loop for c across +standard-chars+
	for s0 = (string c)
	for s = (concatenate 'string "\\" s0)
	for sym = (read-from-string s)
	unless (and (symbolp sym)
		    (string= (symbol-name sym) s0))
	collect (list c s0 s sym))
  nil)

(def-syntax-test syntax.escaped.2
  (let ((count 0))
    (loop for i from 0 below (min 65536 char-code-limit)
	  for c = (code-char i)
	  for s0 = (and c (string c))
	  for s = (and c (concatenate 'string "\\" s0))
	  for sym = (and c (read-from-string s))
	  unless (and c
		      (symbolp sym)
		      (string= (symbol-name sym) s0))
	  collect (progn
		    (when (> (incf count) 100) (loop-finish))
		    (list i c s0 s sym))))
  nil)

(def-syntax-test syntax.escaped.3
  (loop for i = (random (min char-code-limit (ash 1 24)))
	for c = (code-char i)
	for s0 = (and c (string c))
	for s = (and c (concatenate 'string "\\" s0))
	for sym = (and c (read-from-string s))
	repeat 1000
	unless (and c
		    (symbolp sym)
		    (string= (symbol-name sym) s0))
	collect (list i c s0 s sym))
  nil)

(def-syntax-test syntax.escaped.4
  (loop for c across +standard-chars+
	for bad = (find c "\\|")
	for s0 = (string c)
	for s = (concatenate 'string "|" s0 "|")
	for sym = (and (not bad) (read-from-string s))
	unless (or bad
		   (and (symbolp sym)
			(string= (symbol-name sym) s0)))
	collect (list c s0 s sym))
  nil)

(def-syntax-test syntax.escaped.5
  (let ((count 0))
    (loop for i from 0 below (min 65536 char-code-limit)
	  for c = (code-char i)
	  for bad = (find c "\\|")
	  for s0 = (string c)
	  for s = (concatenate 'string "|" s0 "|")
	  for sym = (and (not bad) (read-from-string s))
	  unless (or bad
		     (and (symbolp sym)
			  (string= (symbol-name sym) s0)))
	  collect (progn
		    (when (> (incf count) 100) (loop-finish))
		    (list c s0 s sym))))
  nil)

(def-syntax-test syntax.escaped.6
  (loop for i = (random (min char-code-limit (ash 1 24)))
	for c = (code-char i)
	for bad = (find c "\\|")
	for s0 = (string c)
	for s = (concatenate 'string "|" s0 "|")
	for sym = (and (not bad) (read-from-string s))
	repeat 1000
	unless (or bad
		   (and (symbolp sym)
			(string= (symbol-name sym) s0)))
	collect (list c s0 s sym))
  nil)

(def-syntax-test syntax.escape.whitespace.1
  (let ((names '("Tab" "Newline" "Linefeed" "Space" "Return" "Page"
		 "Rubout" "Backspace")))
    (loop for name in names
	  for c = (name-char name)
	  nconc
	  (when c
	    (let* ((s (concatenate 'string "\\" (string c)))
		   (val (read-from-string s)))
	      (unless (eql val (intern (string c)))
		(list (list name c s val)))))))
  nil)

;;;
;;; CLtS appears to be inconsistent on the next test.
;;; Compare the definition of 'invalid' with the specification
;;; of the token reading algorithm.
;;;
(def-syntax-test syntax.escape.whitespace.2
  (let ((names '("Tab" "Newline" "Linefeed" "Space" "Return" "Page")))
    (loop for name in names
	  for c = (name-char name)
	  nconc
	  (when c
	    (let* ((s (concatenate 'string "|" (string c) "|"))
		   (val (read-from-string s)))
	      (unless (eql val (intern (string c)))
		(list (list name c s val)))))))
  nil)

#|
(def-syntax-test syntax.multiple-escape.invalid.backspace
  (let ((c (name-char "Backspace")))
    (or (not c)
	(let ((s (concatenate 'string "|" (string c) "|")))
	  (eval `(signals-error (read-from-string ',s) reader-error)))))
  t)

(def-syntax-test syntax.multiple-escape.invalid.rubout
  (let ((c (name-char "Rubout")))
    (or (not c)
	(let ((s (concatenate 'string "|" (string c) "|")))
	  (eval `(signals-error (read-from-string ',s) reader-error)))))
  t)
|#


;;; Tests of #\

(def-syntax-test syntax.sharp-backslash.1
  (loop for c across +standard-chars+
	for s = (concatenate 'string "#\\" (string c))
	for c2 = (read-from-string s)
	unless (eql c c2)
	collect (list c s c2))
  nil)

(def-syntax-test syntax.sharp-backslash.2
  (let ((count 0))
    (loop for i below (min 65536 char-code-limit)
	  for c = (code-char i)
	  for s = (and c (concatenate 'string "#\\" (string c)))
	  for c2 = (and c (read-from-string s))
	  unless (eql c c2)
	  collect (progn (when (> (incf count) 100) (loop-finish))
			 (list c s c2))))
  nil)

(def-syntax-test syntax.sharp-backslash.3
  (loop for i = (random (min (ash 1 24) char-code-limit))
	for c = (code-char i)
	for s = (and c (concatenate 'string "#\\" (string c)))
	for c2 = (and c (read-from-string s))
	repeat 1000
	unless (eql c c2)
	collect (list i c s c2))
  nil)

(def-syntax-test syntax.sharp-backslash.4
  (flet ((%f (s) (read-from-string (concatenate 'string "#\\" s))))
    (loop for s in '("SPACE" "NEWLINE" "TAB" "RUBOUT" "BACKSPACE" "PAGE" "LINEFEED" "RETURN")
	  for c = (name-char s)
	  unless (or (null c)
		     (and (eql (%f s) c)
			  (eql (%f (string-downcase s)) c)
			  (eql (%f (string-capitalize s)) c)))
	  collect (list s c)))
  nil)

(def-syntax-test syntax.sharp-backslash.5
  (flet ((%f (s) (read-from-string (concatenate 'string "#\\" s))))
    (loop for c across +standard-chars+
	  for name = (char-name c)
	  unless (or (null name)
		     (and (eql (%f name) c)
			  (eql (%f (string-downcase name)) c)
			  (eql (%f (string-upcase name)) c)
			  (eql (%f (string-capitalize name)) c)))
	  collect (list c name)))
  nil)

(def-syntax-test syntax.sharp-backslash.6
  (flet ((%f (s) (read-from-string (concatenate 'string "#\\" s))))
    (loop for i below (min 65536 char-code-limit)
	  for c = (code-char i)
	  for name = (and c (char-name c))
	  unless (or (null name)
		     (and (eql (%f name) c)
			  (eql (%f (string-downcase name)) c)
			  (eql (%f (string-upcase name)) c)
			  (eql (%f (string-capitalize name)) c)))
	  collect (list i c name)))
  nil)

(def-syntax-test syntax.sharp-backslash.7
  (flet ((%f (s) (read-from-string (concatenate 'string "#\\" s))))
    (loop for i = (random (min (ash 1 24) char-code-limit))
	  for c = (code-char i)
	  for name = (and c (char-name c))
	  repeat 1000
	  unless (or (null name)
		     (and (eql (%f name) c)
			  (eql (%f (string-downcase name)) c)
			  (eql (%f (string-upcase name)) c)
			  (eql (%f (string-capitalize name)) c)))
	  collect (list i c name)))
  nil)


;;; Tests of #'

(def-syntax-test syntax.sharp-quote.1
  (read-from-string "#'X")
  (function |X|) 3)

(def-syntax-test syntax.sharp-quote.2
  (read-from-string "#':X")
  (function :|X|) 4)

(def-syntax-test syntax.sharp-quote.3
  (read-from-string "#'17")
  (function 17) 4)

(def-syntax-test syntax.sharp-quote.error.1
  (signals-error (read-from-string "#'") end-of-file)
  t)

(def-syntax-test syntax.sharp-quote.error.2
  (signals-error (read-from-string "(#'" nil nil) end-of-file)
  t)

;;; Tess of #(...)

(defmacro def-syntax-vector-test (name form &body expected-elements)
  `(def-syntax-test ,name
     (let ((v (read-from-string ,form)))
       (assert (simple-vector-p v))
       v)
     ,(apply #'vector expected-elements)))

(def-syntax-vector-test syntax.sharp-left-paren.1
  "#()")

(def-syntax-vector-test syntax.sharp-left-paren.2
  "#0()")

(def-syntax-vector-test syntax.sharp-left-paren.3
  "#(a)" a)

(def-syntax-vector-test syntax.sharp-left-paren.4
  "#(a b c)" a b c)

(def-syntax-vector-test syntax.sharp-left-paren.5
  "#2(a)" a a)

(def-syntax-vector-test syntax.sharp-left-paren.6
  "#5(a b)" a b b b b)

(def-syntax-vector-test syntax.sharp-left-paren.7
  "#5(a b c d e)" a b c d e)

(def-syntax-vector-test syntax.sharp-left-paren.8
  "#9(a b c d e)" a b c d e e e e e)

(def-syntax-test syntax.sharp-left-paren.9
  (let ((*read-base* 2))
    (read-from-string "#10(a)"))
  #(a a a a a a a a a a)
  6)

(def-syntax-test syntax.sharp-left-paren.error.1
  (signals-error (read-from-string "#(") end-of-file)
  t)

(def-syntax-test syntax.sharp-left-paren.error.2
  (signals-error (read-from-string "(#(" nil nil) end-of-file)
  t)

;;; Tests of #*

(defmacro def-syntax-bit-vector-test (name form &body expected-elements)
  `(def-syntax-test ,name
     (let ((v (read-from-string ,form)))
       (assert (simple-bit-vector-p v))
       v)
     ,(make-array (length expected-elements) :element-type 'bit :initial-contents expected-elements)))

(def-syntax-bit-vector-test syntax.sharp-asterisk.1
  "#*")

(def-syntax-bit-vector-test syntax.sharp-asterisk.2
  "#0*")

(def-syntax-bit-vector-test syntax.sharp-asterisk.3
  "#1*0" 0)

(def-syntax-bit-vector-test syntax.sharp-asterisk.4
  "#1*1" 1)

(def-syntax-bit-vector-test syntax.sharp-asterisk.5
  "#2*1" 1 1)

(def-syntax-bit-vector-test syntax.sharp-asterisk.6
  "#2*0" 0 0)

(def-syntax-bit-vector-test syntax.sharp-asterisk.7
  "#5*010" 0 1 0 0 0)

(def-syntax-bit-vector-test syntax.sharp-asterisk.8
  "#7*0101" 0 1 0 1 1 1 1)

(def-syntax-bit-vector-test syntax.sharp-asterisk.9
  "#10*01010" 0 1 0 1 0 0 0 0 0 0)

(def-syntax-test syntax.sharp-asterisk.10
  (let ((*read-base* 3))
    (read-from-string "#10*01"))
  #*0111111111
  6)

(def-syntax-test syntax.sharp-asterisk.11
  (let ((*read-suppress* t))
    (values (read-from-string "#1* ")))
  nil)

(def-syntax-test syntax.sharp-asterisk.12
  (let ((*read-suppress* t))
    (values (read-from-string "#1*00")))
  nil)

(def-syntax-test syntax.sharp-asterisk.13
  (let ((*read-suppress* t))
    (values (read-from-string "#*012")))
  nil)

(def-syntax-test syntax.sharp-asterisk.error.1
  (signals-error (read-from-string "#1* X") reader-error)
  t)

(def-syntax-test syntax.sharp-asterisk.error.2
  (signals-error (read-from-string "#2*011") reader-error)
  t)

(def-syntax-test syntax.sharp-asterisk.error.3
  (signals-error (read-from-string "#*012") reader-error)
  t)

;;; Tests of #: ...

(defmacro def-syntax-unintern-test (name string)
  `(deftest ,name
     (let ((s (read-from-string ,(concatenate 'string "#:" string))))
       (values
	(symbol-package s)
	(symbol-name s)))
     nil ,(string-upcase string)))

(def-syntax-unintern-test syntax.sharp-colon.1 "")
(def-syntax-unintern-test syntax.sharp-colon.2 "#")
(def-syntax-unintern-test syntax.sharp-colon.3 "a")
(def-syntax-unintern-test syntax.sharp-colon.4 "A")
(def-syntax-unintern-test syntax.sharp-colon.5 "NIL")
(def-syntax-unintern-test syntax.sharp-colon.6 "T")
(def-syntax-unintern-test syntax.sharp-colon.7 ".")


;;; Tests of #.

(def-syntax-test syntax.sharp-dot.1
  (read-from-string "#.(+ 1 2)")
  3 9)

(def-syntax-test syntax.sharp-dot.2
  (read-from-string "#.'X")
  X 4)

(def-syntax-test syntax.sharp-dot.error.1
  (signals-error (read-from-string "#.") end-of-file)
  t)

(def-syntax-test syntax.sharp-dot.error.2
  (signals-error (read-from-string "(#." nil nil) end-of-file)
  t)

(def-syntax-test syntax.sharp-dot.error.3
  (signals-error (let ((*read-eval* nil)) (read-from-string "#.1")) reader-error)
  t)

;;; Tests of #B

(def-syntax-test syntax.sharp-b.1
  (read-from-string "#b0")
  0 3)

(def-syntax-test syntax.sharp-b.2
  (read-from-string "#B1")
  1 3)

(def-syntax-test syntax.sharp-b.3
  (read-from-string "#b101101")
  45 8)

(def-syntax-test syntax.sharp-b.4
  (read-from-string "#B101101")
  45 8)

(def-syntax-test syntax.sharp-b.5
  (read-from-string "#b010001/100")
  17/4 12)

(def-syntax-test syntax.sharp-b.6
  (read-from-string "#b-10011")
  -19 8)

(def-syntax-test syntax.sharp-b.7
  (read-from-string "#B-1/10")
  -1/2 7)

(def-syntax-test syntax.sharp-b.8
  (read-from-string "#B-0/10")
  0 7)

(def-syntax-test syntax.sharp-b.9
  (read-from-string "#b0/111")
  0 7)

(def-syntax-test syntax.sharp-b.10
  (let ((*read-eval* nil))
    (read-from-string "#b-10/11"))
  -2/3 8)

;;; Tests of #O

(def-syntax-test syntax.sharp-o.1
  (read-from-string "#o0")
  0 3)

(def-syntax-test syntax.sharp-o.2
  (read-from-string "#O7")
  7 3)

(def-syntax-test syntax.sharp-o.3
  (read-from-string "#o10")
  8 4)

(def-syntax-test syntax.sharp-o.4
  (read-from-string "#O011")
  9 5)

(def-syntax-test syntax.sharp-o.5
  (read-from-string "#o-0")
  0 4)

(def-syntax-test syntax.sharp-o.6
  (read-from-string "#O-1")
  -1 4)

(def-syntax-test syntax.sharp-o.7
  (read-from-string "#O11/10")
  9/8 7)

(def-syntax-test syntax.sharp-o.8
  (read-from-string "#o-1/10")
  -1/8 7)

(def-syntax-test syntax.sharp-o.9
  (read-from-string "#O0/10")
  0 6)

(def-syntax-test syntax.sharp-o.10
  (let ((*read-eval* nil))
    (read-from-string "#o-10/11"))
  -8/9 8)

;;; Tests of #X

(def-syntax-test syntax.sharp-x.1
  (read-from-string "#x0")
  0 3)

(def-syntax-test syntax.sharp-x.2
  (read-from-string "#X1")
  1 3)

(def-syntax-test syntax.sharp-x.3
  (read-from-string "#xa")
  10 3)

(def-syntax-test syntax.sharp-x.4
  (read-from-string "#Xb")
  11 3)

(def-syntax-test syntax.sharp-x.5
  (read-from-string "#XC")
  12 3)

(def-syntax-test syntax.sharp-x.6
  (read-from-string "#xD")
  13 3)

(def-syntax-test syntax.sharp-x.7
  (read-from-string "#xe")
  14 3)

(def-syntax-test syntax.sharp-x.8
  (read-from-string "#Xf")
  15 3)

(def-syntax-test syntax.sharp-x.9
  (read-from-string "#x10")
  16 4)

(def-syntax-test syntax.sharp-x.10
  (read-from-string "#X1ab")
  427 5)

(def-syntax-test syntax.sharp-x.11
  (read-from-string "#x-1")
  -1 4)

(def-syntax-test syntax.sharp-x.12
  (read-from-string "#X-0")
  0 4)

(def-syntax-test syntax.sharp-x.13
  (read-from-string "#xa/B")
  10/11 5)

(def-syntax-test syntax.sharp-x.14
  (read-from-string "#X-1/1c")
  -1/28 7)

(def-syntax-test syntax.sharp-x.15
  (let ((*read-eval* nil))
    (read-from-string "#x-10/11"))
  -16/17 8)

;;; Tests of #nR

(def-syntax-test syntax.sharp-r.1
  (loop for i = (random (ash 1 (+ 2 (random 32))))
	for base = (+ 2 (random 35))
	for s = (write-to-string i :radix nil :base base :readably nil)
	for c = (random-from-seq "rR")
	for s2 = (format nil "#~d~c~a" base c s)
	for s3 = (rcase (1 (string-upcase s2))
			(1 (string-downcase s2))
			(1 (string-capitalize s2))
			(1 s2))
	for base2 = (+ 2 (random 35))
	for vals = (let ((*read-base* base2))
		     (multiple-value-list
		      (read-from-string s3)))
	repeat 1000
	unless (equal vals (list i (length s3) ))
	collect (list i base s c s2 s3 base2 vals))
  nil)

(def-syntax-test syntax.sharp-r.2
  (read-from-string "#2r0")
  0 4)

(def-syntax-test syntax.sharp-r.3
  (read-from-string "#36r0")
  0 5)

(def-syntax-test syntax.sharp-r.4
  (read-from-string "#29R-0")
  0 6)

(def-syntax-test syntax.sharp-r.5
  (read-from-string "#23r-1")
  -1 6)

(def-syntax-test syntax.sharp-r.6
  (read-from-string "#17r11")
  18 6)

(def-syntax-test syntax.sharp-t.7
  (read-from-string "#3r10/11")
  3/4 8)

(def-syntax-test syntax.sharp-t.8
  (read-from-string "#5R-10/11")
  -5/6 9)

;;; Tests of #c

(def-syntax-test syntax.sharp-c.1
  (read-from-string "#c(1 1)")
  #.(complex 1 1) 7)

(def-syntax-test syntax.sharp-c.2
  (read-from-string "#C(1 0)")
  1 7)

(def-syntax-test syntax.sharp-c.3
  (read-from-string "#c(0 1)")
  #.(complex 0 1) 7)

(def-syntax-test syntax.sharp-c.4
  (read-from-string "#c(-1/2 1)")
  #.(complex -1/2 1) 10)

(def-syntax-test syntax.sharp-c.5
  (read-from-string "#c (1 1)")
  #.(complex 1 1) 8)
