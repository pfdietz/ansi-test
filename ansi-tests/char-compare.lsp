;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Oct  5 19:36:00 2002
;;;; Contains: Tests of character comparison functions

(in-package :cl-test)

;;; The character comparisons should throw a PROGRAM-ERROR when
;;; safe-called with no arguments
(deftest char-compare-no-args
  (loop for f in '(char= char/= char< char> char<= char>=
		   char-lessp char-greaterp char-equal
		   char-not-lessp char-not-greaterp char-not-equal)
	collect (handler-case (funcall f) (program-error () :caught)))
  (:caught :caught :caught :caught :caught :caught
   :caught :caught :caught :caught :caught :caught))

(deftest char=.1
  (is-ordered-by +code-chars+ #'(lambda (c1 c2) (not (char= c1 c2))))
  t)

(deftest char=.2
  (loop for c across +code-chars+
	always (char= c c))
  t)

(deftest char=.3
  (every #'char= +code-chars+)
  t)

(deftest char=.4
  (is-ordered-by +rev-code-chars+
		 #'(lambda (c1 c2) (not (char= c1 c2))))
  t)

(deftest char/=.1
  (is-ordered-by +code-chars+ #'char/=)
  t)

(deftest char/=.2
  (loop for c across +code-chars+
	never (char/= c c))
  t)

(deftest char/=.3
  (every #'char/= +code-chars+)
  t)

(deftest char/=.4
  (is-ordered-by +rev-code-chars+ #'char/=)
  t)

(deftest char<=.1
  (loop for c across +code-chars+
	always (char<= c c))
  t)

(deftest char<=.2
  (every #'char<= +code-chars+)
  t)

(deftest char<=.3
  (is-antisymmetrically-ordered-by +code-chars+ #'char<=)
  t)

(deftest char<=.4
  (is-antisymmetrically-ordered-by +lower-case-chars+ #'char<=)
  t)

(deftest char<=.5
  (is-antisymmetrically-ordered-by +upper-case-chars+ #'char<=)
  t)

(deftest char<=.6
  (is-antisymmetrically-ordered-by +digit-chars+ #'char<=)
  t)

(deftest char<=.7
  (or (char<= #\9 #\A) (char<= #\Z #\0) t)
  t)
  
(deftest char<=.8
  (or (char<= #\9 #\a) (char<= #\z #\0) t)
  t)

(deftest char<.1
  (loop for c across +code-chars+
	never (char< c c))
  t)

(deftest char<.2
  (every #'char< +code-chars+)
  t)

(deftest char<.3
  (is-antisymmetrically-ordered-by +code-chars+ #'char<)
  t)

(deftest char<.4
  (is-antisymmetrically-ordered-by +lower-case-chars+ #'char<)
  t)

(deftest char<.5
  (is-antisymmetrically-ordered-by +upper-case-chars+ #'char<)
  t)

(deftest char<.6
  (is-antisymmetrically-ordered-by +digit-chars+ #'char<)
  t)

(deftest char<.7
  (or (char< #\9 #\A) (char< #\Z #\0) t)
  t)
  
(deftest char<.8
  (or (char< #\9 #\a) (char< #\z #\0) t)
  t)
  

(deftest char>=.1
  (loop for c across +code-chars+
	always (char>= c c))
  t)

(deftest char>=.2
  (every #'char>= +code-chars+)
  t)

(deftest char>=.3
  (is-antisymmetrically-ordered-by +rev-code-chars+ #'char>=)
  t)

(deftest char>=.4
  (is-antisymmetrically-ordered-by (reverse +lower-case-chars+) #'char>=)
  t)

(deftest char>=.5
  (is-antisymmetrically-ordered-by (reverse +upper-case-chars+) #'char>=)
  t)

(deftest char>=.6
  (is-antisymmetrically-ordered-by (reverse +digit-chars+) #'char>=)
  t)

(deftest char>=.7
  (or (char>= #\A #\9) (char>= #\0 #\Z) t)
  t)
  
(deftest char>=.8
  (or (char>= #\a #\9) (char>= #\0 #\z) t)
  t)

(deftest char>.1
  (loop for c across +code-chars+
	never (char> c c))
  t)

(deftest char>.2
  (every #'char> +code-chars+)
  t)

(deftest char>.3
  (is-antisymmetrically-ordered-by +rev-code-chars+ #'char>)
  t)

(deftest char>.4
  (is-antisymmetrically-ordered-by (reverse +lower-case-chars+) #'char>)
  t)

(deftest char>.5
  (is-antisymmetrically-ordered-by (reverse +upper-case-chars+) #'char>)
  t)

(deftest char>.6
  (is-antisymmetrically-ordered-by (reverse +digit-chars+) #'char>)
  t)

(deftest char>.7
  (or (char> #\A #\9) (char> #\0 #\Z) t)
  t)
  
(deftest char>.8
  (or (char> #\a #\9) (char> #\0 #\z) t)
  t)

;;; Case-insensitive comparisons

(deftest char-equal.1
  (is-ordered-by +code-chars+
		#'(lambda (c1 c2)
		    (or (char= (char-downcase c1)
			       (char-downcase c2))
			(not (char-equal c1 c2)))))
  t)

(deftest char-equal.2
  (loop for c across +code-chars+
	always (char-equal c c))
  t)

(deftest char-equal.3
  (loop for c across +code-chars+
	always (char-equal c))
  t)

(deftest char-equal.4
  (is-ordered-by +rev-code-chars+
		 #'(lambda (c1 c2)
		     (or (char= (char-downcase c1)
				(char-downcase c2))
			 (not (char-equal c1 c2)))))
  t)

(deftest char-not-equal.1
  (is-ordered-by +code-chars+ #'(lambda (c1 c2)
				  (or (char= (char-downcase c1)
					     (char-downcase c2))
				      (char-not-equal c1 c2))))
  t)

(deftest char-not-equal.2
  (loop for c across +code-chars+
	never (char-not-equal c c))
  t)

(deftest char-not-equal.3
  (every #'char-not-equal +code-chars+)
  t)

(deftest char-not-equal.4
  (is-ordered-by +rev-code-chars+ #'(lambda (c1 c2)
				      (or (char= (char-downcase c1)
						 (char-downcase c2))
					  (char-not-equal c1 c2))))
  t)

(deftest char-not-greaterp.1
  (loop for c across +code-chars+
	always (char-not-greaterp c c))
  t)

(deftest char-not-greaterp.2
  (every #'char-not-greaterp +code-chars+)
  t)

(deftest char-not-greaterp.3
  (is-case-insensitive #'char-not-greaterp)
  t)

(deftest char-not-greaterp.4
  (is-antisymmetrically-ordered-by +lower-case-chars+ #'char-not-greaterp)
  t)

(deftest char-not-greaterp.5
  (is-antisymmetrically-ordered-by +upper-case-chars+ #'char-not-greaterp)
  t)

(deftest char-not-greaterp.6
  (is-antisymmetrically-ordered-by +digit-chars+ #'char-not-greaterp)
  t)

(deftest char-not-greaterp.7
  (or (char-not-greaterp #\9 #\A) (char-not-greaterp #\Z #\0) t)
  t)
  
(deftest char-not-greaterp.8
  (or (char-not-greaterp #\9 #\a) (char-not-greaterp #\z #\0) t)
  t)

(deftest char-lessp.1
  (loop for c across +code-chars+
	never (char-lessp c c))
  t)

(deftest char-lessp.2
  (every #'char-lessp +code-chars+)
  t)

(deftest char-lessp.3
  (is-case-insensitive #'char-lessp)
  t)

(deftest char-lessp.4
  (is-antisymmetrically-ordered-by +lower-case-chars+ #'char-lessp)
  t)

(deftest char-lessp.5
  (is-antisymmetrically-ordered-by +upper-case-chars+ #'char-lessp)
  t)

(deftest char-lessp.6
  (is-antisymmetrically-ordered-by +digit-chars+ #'char-lessp)
  t)

(deftest char-lessp.7
  (or (char-lessp #\9 #\A) (char-lessp #\Z #\0) t)
  t)
  
(deftest char-lessp.8
  (or (char-lessp #\9 #\a) (char-lessp #\z #\0) t)
  t)

(deftest char-not-lessp.1
  (loop for c across +code-chars+
	always (char-not-lessp c c))
  t)

(deftest char-not-lessp.2
  (every #'char-not-lessp +code-chars+)
  t)

(deftest char-not-lessp.3
  (is-case-insensitive #'char-not-lessp)
  t)

(deftest char-not-lessp.4
  (is-antisymmetrically-ordered-by (reverse +lower-case-chars+)
				   #'char-not-lessp)
  t)

(deftest char-not-lessp.5
  (is-antisymmetrically-ordered-by (reverse +upper-case-chars+) #'char-not-lessp)
  t)

(deftest char-not-lessp.6
  (is-antisymmetrically-ordered-by (reverse +digit-chars+) #'char-not-lessp)
  t)

(deftest char-not-lessp.7
  (or (char-not-lessp #\A #\9) (char-not-lessp #\0 #\Z) t)
  t)
  
(deftest char-not-lessp.8
  (or (char-not-lessp #\a #\9) (char-not-lessp #\0 #\z) t)
  t)

(deftest char-greaterp.1
  (loop for c across +code-chars+
	never (char-greaterp c c))
  t)

(deftest char-greaterp.2
  (every #'char-greaterp +code-chars+)
  t)

(deftest char-greaterp.3
  (is-case-insensitive #'char-greaterp)
  t)

(deftest char-greaterp.4
  (is-antisymmetrically-ordered-by (reverse +lower-case-chars+)
				   #'char-greaterp)
  t)

(deftest char-greaterp.5
  (is-antisymmetrically-ordered-by (reverse +upper-case-chars+) #'char-greaterp)
  t)

(deftest char-greaterp.6
  (is-antisymmetrically-ordered-by (reverse +digit-chars+) #'char-greaterp)
  t)

(deftest char-greaterp.7
  (or (char-greaterp #\A #\9) (char-greaterp #\0 #\Z) t)
  t)
  
(deftest char-greaterp.8
  (or (char-greaterp #\a #\9) (char-greaterp #\0 #\z) t)
  t)
