;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Oct  5 12:52:18 2002
;;;; Contains: Tests associated with the class CHARACTER

(in-package :cl-test)

(defun char-type-error-check (fn)
  (loop for x in *universe*
	always (or (characterp x)
		   (eqt (catch-type-error (funcall fn x)) 'type-error))))

(deftest character-class.1
  (subtypep* 'character t)
  t t)

(deftest base-char.1
  (subtypep* 'base-char 'character)
  t t)

(deftest base-char.2
  (subtypep* 'base-char t)
  t t)

(deftest base-char.3
  (every #'(lambda (c) (typep c 'base-char)) +standard-chars+)
  t)

(deftest standard-char.1
  (subtypep* 'standard-char 'base-char)
  t t)

(deftest standard-char.2
  (subtypep* 'standard-char 'character)
  t t)

(deftest standard-char.3
  (subtypep* 'standard-char t)
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
  (subtypep* 'extended-char 'character)
  t t)

(deftest extended-char.2
  (subtypep* 'extended-char t)
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
  (char-type-error-check #'alpha-char-p)
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
  (char-type-error-check #'alphanumericp)
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
  (char-type-error-check #'graphic-char-p)
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

(deftest standard-char-p.2a
  (loop for i from 0 below (min 65536 char-code-limit)
	for x = (code-char i)
	always (or (not (characterp x))
		   (find x +standard-chars+)
		   (not (standard-char-p x))))
  t)

(deftest standard-char-p.3
  (char-type-error-check #'standard-char-p)
  t)

(deftest char-upcase.1
  (loop for x in *universe*
	always
	(or (not (characterp x))
	    (let ((u (char-upcase x))) 
	      (and
	       (or (lower-case-p x) (eql u x))
	       (eql u (char-upcase u))))))
  t)

(deftest char-upcase.2
  (loop for i from 0 below (min 65536 char-code-limit)
	for x = (code-char i)
	always
	(or (not x)
	    (let ((u (char-upcase x)))
	      (and
	       (or (lower-case-p x) (eql u x))
	       (eql u (char-upcase u))))))
  t)

(deftest char-upcase.3
  (map 'string #'char-upcase +alpha-chars+)
  "ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ")

(deftest char-upcase.4
  (char-type-error-check #'char-upcase)
  t)	

(deftest char-downcase.1
  (loop for x in *universe*
	always
	(or (not (characterp x))
	    (let ((u (char-downcase x))) 
	      (and
	       (or (upper-case-p x) (eql u x))
	       (eql u (char-downcase u))))))
  t)

(deftest char-downcase.2
  (loop for i from 0 below (min 65536 char-code-limit)
	for x = (code-char i)
	always
	(or (not x)
	    (let ((u (char-downcase x)))
	      (and
	       (or (upper-case-p x) (eql u x))
	       (eql u (char-downcase u))))))
  t)

(deftest char-downcase.3
  (map 'string #'char-downcase +alpha-chars+)
  "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz")

(deftest char-downcase.4
  (char-type-error-check #'char-downcase)
  t)

(deftest upper-case-p.1
  (find-if-not #'upper-case-p +standard-chars+ :start 26 :end 52)
  nil)

(deftest upper-case-p.2
  (find-if #'upper-case-p +standard-chars+ :end 26)
  nil)

(deftest upper-case-p.3
  (find #'upper-case-p +standard-chars+ :start 52)
  nil)

(deftest upper-case-p.4
  (char-type-error-check #'upper-case-p)
  t)

(deftest lower-case-p.1
  (find-if-not #'lower-case-p +standard-chars+ :end 26)
  nil)

(deftest lower-case-p.2
  (find-if #'lower-case-p +standard-chars+ :start 26)
  nil)

(deftest lower-case-p.3
  (char-type-error-check #'lower-case-p)
  t)

(deftest both-case-p.1
  (loop for x in *universe*
	always (or (not (characterp x))
		   (if (both-case-p x)
		       (and (graphic-char-p x)
			    (or (upper-case-p x)
				(lower-case-p x)))
		     (not (or (upper-case-p x)
			      (lower-case-p x))))))
  t)

(deftest both-case-p.2
  (loop for i from 0 below (min 65536 char-code-limit)
	for x = (code-char i)
	always (or (not (characterp x))
		   (if (both-case-p x)
		       (and (graphic-char-p x)
			    (or (upper-case-p x)
				(lower-case-p x)))
		     (not (or (upper-case-p x)
			      (lower-case-p x))))))
  t)

(deftest both-case-p.3
  (char-type-error-check #'both-case-p)
  t)

(deftest char-code.1
  (char-type-error-check #'char-code)
  t)

(deftest char-code.2
  (loop for i from 0 below (min 65536 char-code-limit)
	for c = (code-char i)
	always (or (not c)
		   (eql (char-code c) i)))
  t)

(deftest code-char.1
  (loop for x across +standard-chars+
	always (eql (code-char (char-code x)) x))
  t)

(deftest char-int.1
  (loop for x across +standard-chars+
	always (eql (char-int x) (char-code x)))
  t)

(deftest char-int.2
  (let ((c->i (make-hash-table :test #'equal))
	(i->c (make-hash-table :test #'eql)))
    (flet ((%insert
	    (c)
	    (or (not (characterp c))
		(let* ((i (char-int c))
		       (j (gethash c c->i))
		       (d (gethash i i->c)))
		  (and
		   (or (null j) (eql j i))
		   (or (null d) (char= c d))
		   (progn
		     (setf (gethash c c->i) i)
		     (setf (gethash i i->c) c)
		     t))))))
      (and
       (loop for i from 0 below char-code-limit
	     always (%insert (code-char i)))
       (every #'%insert +standard-chars+)
       (every #'%insert *universe*))))
  t)

(deftest char-name.1
  (flet ((%check
	  (c)
	  (or (not (characterp c))
	      (let ((name (char-name c)))
		(or (null name)
		    (and (stringp name)
			 (eql c (name-char name))))))))
    (and
     (loop for i from 0 below char-code-limit
	   always (%check (code-char i)))
     (every #'%check +standard-chars+)
     (every #'%check *universe*)))
  t)

(deftest char-name.2
  (string= (char-name #\Space) "Space")
  t)

(deftest char-name.3
  (string= (char-name #\Newline) "Newline")
  t)

;;; Check that the names of various semi-standard characters are
;;; appropriate.  This is complicated by the possibility that two different
;;; names may refer to the same character (as is allowed by the standard,
;;; for example in the case of Newline and Linefeed).
 
(deftest char-name.4
  (loop for s in '("Rubout" "Page" "Backspace" "Return" "Tab" "Linefeed")
	for c = (name-char s)
	unless (or (not c)
		   ;; If the char-name is not even string-equal,
		   ;; assume we're sharing the character with some other
		   ;; name, and assume it's ok
		   (not (string-equal (char-name c) s))
		   (string= (char-name c) s))
	;; Collect list of cases that failed
	collect (list s c (char-name c)))
  nil)

(deftest char-name.5
  (char-type-error-check #'char-name)
  t)

(deftest name-char.1
  (loop for x in *universe*
	for s = (catch-type-error (string x))
	always
	(or (eql s 'type-error)
	    (let ((c (name-char x)))
	      (or (not c)
		  (characterp c)
		  (string-equal (char-name c) s)))))
  t)

(deftest name-char.2
  (loop for s in '("RubOut" "PAGe" "BacKspace" "RetUrn" "Tab" "LineFeed"
		   "SpaCE" "NewLine")
	always
	(let ((c1 (name-char (string-upcase s)))
	      (c2 (name-char (string-downcase s)))
	      (c3 (name-char (string-capitalize s)))
	      (c4 (name-char s)))
	  (and (eql c1 c2) (eql c2 c3) (eql c3 c4))))
  t)
