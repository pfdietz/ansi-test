;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Oct  5 12:52:18 2002
;;;; Contains: Tests associated with the class CHARACTER

(in-package :cl-test)

(deftest character-class.1
  (subtypep 'character t)
  t t)

(deftest base-char.1
  (subtypep 'base-char 'character)
  t t)

(deftest base-char.2
  (subtypep 'base-char t)
  t t)

(deftest base-char.3
  (every #'(lambda (c) (typep c 'base-char)) +standard-chars+)
  t)

(deftest standard-char.1
  (subtypep 'standard-char 'base-char)
  t t)

(deftest standard-char.2
  (subtypep 'standard-char 'character)
  t t)

(deftest standard-char.3
  (subtypep 'standard-char t)
  t t)

(deftest standard-char.4
  (every #'(lambda (c) (typep c 'standard-char)) +standard-chars+)
  t)

(deftest standard-char.5
  (loop for i from 0 below (min 65536 char-code-limit)
	always (let ((c (code-char i)))
		 (not (and (typep c 'standard-char)
			   (not (standard-char-p c))))))
  t)

(deftest extended-char.1
  (subtypep 'extended-char 'character)
  t t)

(deftest extended-char.2
  (subtypep 'extended-char t)
  t t)

(deftest extended-char.3
  (loop for i from 0 below (min 65536 char-code-limit)
	always (let ((c (code-char i)))
		 (not (and (typep c 'extended-char)
			   (typep c 'base-char)))))
  t)

;;; 

(deftest character.1
  (loop for i from 0 below (min 65536 char-code-limit)
	always (let ((c (code-char i)))
		 (or (null c)
		     (let ((s (string c)))
		       (and
			(eql (character c) c)
			(eql (character s) c)
			(eql (character (make-symbol s)) c))))))
  t)

(deftest character.2
  (loop for x in *universe*
	when (not (or (characterp x)
		      (and (stringp x) (eql (length x) 1))
		      (and (symbolp x) (eql (length (symbol-name x)) 1))
		      (let ((c (catch-type-error (character x))))
			(or (eql c 'type-error)
			    (let ((s (catch-type-error (string x))))
			      (and (stringp s) (eql (char s 0) c)))))))
	do (return x))
  nil)

(deftest characterp.1
  (every #'characterp +standard-chars+)
  t)

(deftest characterp.2
  (loop for i from 0 below (min 65536 char-code-limit)
	always (let ((c (code-char i)))
		 (or (null c) (characterp c))))
  t)

(deftest characterp.3
  (loop for x in *universe*
	always (let ((p (characterp x))
		     (q (typep x 'character)))
		 (if p (not (not q)) (not q))))
  t)

(deftest alpha-char-p.1
  (loop for c across +standard-chars+
	always
	(or (find c +alpha-chars+)
	    (not (alpha-char-p c))))
  t)

(deftest alpha-char-p.2
  (every #'alpha-char-p +alpha-chars+)
  t)

(deftest alpha-char-p.3
  (loop for x in *universe*
	always (if (characterp x)
		   (or (find x +alpha-chars+)
		       (not (alpha-char-p x)))
		 (eql (catch-type-error (alpha-char-p x)) 'type-error)))
  t)

(deftest alphanumericp.1
  (loop for c across +standard-chars+
	always
	(or (find c +alphanumeric-chars+)
	    (not (alphanumericp c))))
  t)

(deftest alphanumericp.2
  (every #'alphanumericp +alphanumeric-chars+)
  t)

(deftest alphanumericp.3
  (loop for x in *universe*
	always (if (characterp x)
		   (or (find x +alphanumeric-chars+)
		       (not (alphanumericp x)))
		 (eql (catch-type-error (alphanumericp x)) 'type-error)))
  t)

(deftest alphanumericp.4
  (loop for x in *universe*
	always (or (not (characterp x))
		   (if (or (digit-char-p x) (alpha-char-p x))
		       (alphanumericp x)
		     (not (alphanumericp x)))))
  t)

(deftest alphanumericp.5
  (loop for i from 0 below (min 65536 char-code-limit)
	for x = (code-char i)
	always (or (not (characterp x))
		   (if (or (digit-char-p x) (alpha-char-p x))
		       (alphanumericp x)
		     (not (alphanumericp x)))))
  t)

(deftest digit-char.1
  (loop
   for r from 2 to 36
   always
   (loop for i from 0 to 36
	 always (let ((c (digit-char i r)))
		  (if (>= i r) (null c)
		    (eql c (char +extended-digit-chars+ i))))))
  t)

(deftest digit-char.2
  (map 'list #'digit-char (loop for i from 0 to 39 collect i))
  (#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
   nil nil nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil nil nil))

(deftest digit-char-p.1
  (loop for x in *universe*
	always (not (and (characterp x)
			 (not (alphanumericp x))
			 (digit-char-p x))))
  t)

(deftest digit-char-p.2
  (loop for i from 0 below (min 65536 char-code-limit)
	for x = (code-char i)
	always (or (not x)
		   (not (and (not (alphanumericp x))
			     (digit-char-p x)))))
  t)
		   
(deftest digit-char-p.3
  (loop for r from 2 to 35
	always
	(loop for i from r to 35
	      for c = (char +extended-digit-chars+ i)
	      never (or (digit-char-p c r)
			(digit-char-p (char-downcase c) r))))
  t)

(deftest digit-char-p.4
  (loop for r from 2 to 35
	always
	(loop for i from 0 below r
	      for c = (char +extended-digit-chars+ i)
	      always (and (eql (digit-char-p c r) i)
			  (eql (digit-char-p (char-downcase c) r) i))))
  t)

(deftest digit-char-p.5
  (loop for i from 10 to 35
	for c = (char +extended-digit-chars+ i)
	never (or (digit-char-p c)
		  (digit-char-p (char-downcase c))))
  t)

(deftest digit-char-p.6
  (loop for i from 0 below 10
	for c = (char +extended-digit-chars+ i)
	always (eql (digit-char-p c) i))
  t)

(deftest graphic-char-p.1
  (loop for c across +standard-chars+
	always (if (eql c #\Newline)
		   (not (graphic-char-p c))
		 (graphic-char-p c)))
  t)

(deftest graphic-char.2
  (loop
   for name in '("Rubout" "Page" "Backspace" "Tab" "Linefeed" "Return")
   for c = (name-char name)
   when (and c (graphic-char-p c)) collect c)
  nil)

(deftest graphic-char.3
  (loop for x in *universe*
	never (and (not (characterp x))
		   (not (eq (catch-type-error (graphic-char-p x)) 'type-error))))
  t)

(deftest standard-char-p.1
  (every #'standard-char-p +standard-chars+)
  t)

(deftest standard-char-p.2
  (loop for x in *universe*
	always (or (not (characterp x))
		   (find x +standard-chars+)
		   (not (standard-char-p x))))
  t)

(deftest standard-char-p.2
  (loop for i from 0 below (min 65536 char-code-limit)
	for x = (code-char i)
	always (or (not (characterp x))
		   (find x +standard-chars+)
		   (not (standard-char-p x))))
  t)

(deftest standard-char-p.3
  (loop for x in *universe*
	always (or (characterp x)
		   (eq (catch-type-error (standard-char-p x)) 'type-error)))
  t)
