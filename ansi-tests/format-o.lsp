;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Aug  1 06:36:30 2004
;;;; Contains: Tests of format directive ~O

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(deftest format.o.1
  (with-standard-io-syntax
   (loop for x = (ash 1 (+ 2 (random 80)))
	 for i = (- (random (+ x x)) x)
	 for s1 = (format nil "~O" i)
	 for j = (let ((*read-base* 8)) (read-from-string s1))
	 repeat 1000
	 when (or (/= i j)
		  (find #\. s1)
		  (find #\+ s1)
		  (find-if #'alpha-char-p s1))
	 collect (list i s1 j)))
  nil)

(deftest format.o.2
  (with-standard-io-syntax
   (loop for x = (ash 1 (+ 2 (random 80)))
	 for i = (- (random (+ x x)) x)
	 for s1 = (format nil "~@o" i)
	 for j = (let ((*read-base* 8)) (read-from-string s1))
	 repeat 1000
	 when (or (/= i j)
		  (find #\. s1)
		  ;; (find #\+ s1)
		  (find-if #'alpha-char-p s1))
	 collect (list i s1 j)))
  nil)

(deftest format.o.3
  (with-standard-io-syntax
   (loop for x = (ash 1 (+ 2 (random 80)))
	 for mincol = (random 30)
	 for i = (- (random (+ x x)) x)
	 for s1 = (format nil "~o" i)
	 for s2 = (format nil (format nil "~~~do" mincol) i)
	 for pos = (search s1 s2)
	 repeat 1000
	 when (or (null pos)
		  (and (> mincol (length s1))
		       (or (/= (length s2) mincol)
			   (not (eql (position #\Space s2 :test-not #'eql)
				     (- (length s2) (length s1)))))))
	 collect (list i mincol s1 s2 pos)))
  nil)

(deftest format.o.4
  (with-standard-io-syntax
   (loop for x = (ash 1 (+ 2 (random 80)))
	 for mincol = (random 30)
	 for i = (- (random (+ x x)) x)
	 for s1 = (format nil "~@O" i)
	 for s2 = (format nil (format nil "~~~d@o" mincol) i)
	 for pos = (search s1 s2)
	 repeat 1000
	 when (or (null pos)
		  (and (>= i 0) (not (eql (elt s1 0) #\+)))
		  (and (> mincol (length s1))
		       (or (/= (length s2) mincol)
			   (not (eql (position #\Space s2 :test-not #'eql)
				     (- (length s2) (length s1)))))))
	 collect (list i mincol s1 s2 pos)))
  nil)

(deftest format.o.5
  (with-standard-io-syntax
   (loop for x = (ash 1 (+ 2 (random 80)))
	 for mincol = (random 30)
	 for padchar = (random-from-seq +standard-chars+)
	 for i = (- (random (+ x x)) x)
	 for s1 = (format nil "~o" i)
	 for s2 = (format nil (format nil "~~~d,'~c~c" mincol padchar
				      (random-from-seq "oO"))
			  i)
	 for pos = (search s1 s2)
	 repeat 1000
	 when (or (null pos)
		  (and (> mincol (length s1))
		       (or (/= (length s2) mincol)
			   (find padchar s2 :end (- (length s2) (length s1))
				 :test-not #'eql))))
	 collect (list i mincol s1 s2 pos)))
  nil)

(deftest format.o.6
  (with-standard-io-syntax
   (loop for x = (ash 1 (+ 2 (random 80)))
	 for mincol = (random 30)
	 for padchar = (random-from-seq +standard-chars+)
	 for i = (- (random (+ x x)) x)
	 for s1 = (format nil "~o" i)
	 for s2 = (format nil "~v,vO" mincol padchar i)
	 for pos = (search s1 s2)
	 repeat 1000
	 when (or (null pos)
		  (and (> mincol (length s1))
		       (or (/= (length s2) mincol)
			   (find padchar s2 :end (- (length s2) (length s1))
				 :test-not #'eql))))
	 collect (list i mincol s1 s2 pos)))
  nil)

(deftest format.o.7
  (with-standard-io-syntax
   (loop for x = (ash 1 (+ 2 (random 80)))
	 for mincol = (random 30)
	 for padchar = (random-from-seq +standard-chars+)
	 for i = (- (random (+ x x)) x)
	 for s1 = (format nil "~@o" i)
	 for s2 = (format nil "~v,v@o" mincol padchar i)
	 for pos = (search s1 s2)
	 repeat 1000
	 when (or (null pos)
		  (and (>= i 0) (not (eql (elt s1 0) #\+)))
		  (and (> mincol (length s1))
		       (or (/= (length s2) mincol)
			   (find padchar s2 :end (- (length s2) (length s1))
				 :test-not #'eql))))
	 collect (list i mincol s1 s2 pos)))
  nil)

;;; Comma tests

(deftest format.o.8
  (loop for i from #o-777 to #o777
	for s1 = (format nil "~o" i)
	for s2 = (format nil "~:o" i)
	unless (string= s1 s2)
	collect (list i s1 s2))
  nil)

(deftest format.o.9
  (with-standard-io-syntax
   (loop for x = (ash 1 (+ 2 (random 80)))
	 for i = (- (random (+ x x)) x)
	 for commachar = #\,
	 for s1 = (format nil "~o" i)
	 for s2 = (format nil "~:o" i)
	 repeat 1000
	 unless (and (string= s1 (remove commachar s2))
		     (not (eql (elt s2 0) commachar))
		     (or (>= i 0) (not (eql (elt s2 1) commachar)))
		     (let ((len (length s2))
			   (ci+1 4))
		       (loop for i from (if (< i 0) 2 1) below len
			     always (if (= (mod (- len i) ci+1) 0)
					(eql (elt s2 i) commachar)
				      (find (elt s2 i) "01234567")))))
	 collect (list x i commachar s1 s2)))
  nil)

(deftest format.o.10
  (with-standard-io-syntax
   (loop for x = (ash 1 (+ 2 (random 80)))
	 for i = (- (random (+ x x)) x)
	 for commachar = (random-from-seq +standard-chars+)
	 for s1 = (format nil "~o" i)
	 for s2 = (format nil "~,,v:o" commachar i)
	 repeat 1000
	 unless (and
		 (eql (elt s1 0) (elt s2 0))
		 (if (< i 0) (eql (elt s1 1) (elt s2 1)) t)
		 (let ((len (length s2))
		      (ci+1 4)
		      (j (if (< i 0) 1 0)))
		  (loop for i from (if (< i 0) 2 1) below len
			always (if (= (mod (- len i) ci+1) 0)
				   (eql (elt s2 i) commachar)
				 (eql (elt s1 (incf j)) (elt s2 i))))))
	 collect (list x i commachar s1 s2)))
  nil)

(deftest format.o.11
  (with-standard-io-syntax
   (loop for x = (ash 1 (+ 2 (random 80)))
	 for i = (- (random (+ x x)) x)
	 for commachar = (random-from-seq +standard-chars+)
	 for s1 = (format nil "~o" i)
	 for s2 = (format nil (format nil "~~,,'~c:~c" commachar
				      (random-from-seq "oO"))
			  i)
	 repeat 1000
	 unless (and
		 (eql (elt s1 0) (elt s2 0))
		 (if (< i 0) (eql (elt s1 1) (elt s2 1)) t)
		 (let ((len (length s2))
		      (ci+1 4)
		      (j (if (< i 0) 1 0)))
		  (loop for i from (if (< i 0) 2 1) below len
			always (if (= (mod (- len i) ci+1) 0)
				   (eql (elt s2 i) commachar)
				 (eql (elt s1 (incf j)) (elt s2 i))))))
	 collect (list x i commachar s1 s2)))
  nil)

(deftest format.o.12
  (with-standard-io-syntax
   (loop for x = (ash 1 (+ 2 (random 80)))
	 for i = (- (random (+ x x)) x)
	 for commachar = (random-from-seq +standard-chars+)
	 for commaint = (1+ (random 20))
	 for s1 = (format nil "~o" i)
	 for s2 = (format nil "~,,v,v:O" commachar commaint i)
	 repeat 1000
	 unless (and
		 (eql (elt s1 0) (elt s2 0))
		 (if (< i 0) (eql (elt s1 1) (elt s2 1)) t)
		 (let ((len (length s2))
		       (ci+1 (1+ commaint))
		       (j (if (< i 0) 1 0)))
		   (loop for i from (if (< i 0) 2 1) below len
			 always (if (= (mod (- len i) ci+1) 0)
				    (eql (elt s2 i) commachar)
				  (eql (elt s1 (incf j)) (elt s2 i))))))
	 collect (list x i commachar s1 s2)))
  nil)

(deftest format.o.13
  (with-standard-io-syntax
   (loop for x = (ash 1 (+ 2 (random 80)))
	 for i = (- (random (+ x x)) x)
	 for commachar = (random-from-seq +standard-chars+)
	 for commaint = (1+ (random 20))
	 for s1 = (format nil "~@o" i)
	 for s2 = (format nil "~,,v,v:@o" commachar commaint i)
	 repeat 1000
	 unless (and
		 (eql (elt s1 0) (elt s2 0))
		 (eql (elt s1 1) (elt s2 1))
		 (let ((len (length s2))
		       (ci+1 (1+ commaint))
		       (j 1))
		   (loop for i from 2 below len
			 always (if (= (mod (- len i) ci+1) 0)
				    (eql (elt s2 i) commachar)
				  (eql (elt s1 (incf j)) (elt s2 i))))))
	 collect (list x i commachar s1 s2)))
  nil)

;;; NIL arguments

(def-format-test format.o.14
  "~vO" (nil #o100) "100")

(def-format-test format.o.15
  "~6,vO" (nil #o100) "   100")

(def-format-test format.o.16
  "~,,v:o" (nil #o12345) "12,345")

(def-format-test format.o.17
  "~,,'*,v:o" (nil #o12345) "12*345")

;;; When the argument is not an integer, print as if using ~A and base 10

(deftest format.o.18
  (loop for x in *mini-universe*
	for s1 = (format nil "~o" x)
	for s2 = (let ((*print-base* 8)) (format nil "~A" x))
	unless (or (integerp x) (string= s1 s2))
	collect (list x s1 s2))
  nil)

(deftest format.o.19
  (loop for x in *mini-universe*
	for s1 = (format nil "~:o" x)
	for s2 = (let ((*print-base* 8)) (format nil "~A" x))
	unless (or (integerp x) (string= s1 s2))
	collect (list x s1 s2))
  nil)

(deftest format.o.20
  (loop for x in *mini-universe*
	for s1 = (format nil "~@o" x)
	for s2 = (let ((*print-base* 8)) (format nil "~A" x))
	unless (or (integerp x) (string= s1 s2))
	collect (list x s1 s2))
  nil)

(deftest format.o.21
  (loop for x in *mini-universe*
	for s1 = (format nil "~@:o" x)
	for s2 = (let ((*print-base* 8)) (format nil "~A" x))
	unless (or (integerp x) (string= s1 s2))
	collect (list x s1 s2))
  nil)

;;; Must add tests for non-integers when the parameters
;;; are specified, but it's not clear what the meaning is.
;;; Does mincol apply to the ~A equivalent?  What about padchar?
;;; Are comma-char and comma-interval always ignored?

;;; # arguments

(deftest format.o.22
  (apply
   #'values
   (loop for i from 0 to 10
	 for args = (make-list i)
	 for s = (apply #'format nil "~#o" #o12345 args)
	 collect s))
  "12345"
  "12345"
  "12345"
  "12345"
  "12345"
  " 12345"
  "  12345"
  "   12345"
  "    12345"
  "     12345"
  "      12345")

(deftest format.o.23
  (apply
   #'values
   (loop for i from 0 to 10
	 for args = (make-list i)
	 for s = (apply #'format nil "~,,,#:o" #o1234567012 args)
	 collect s))
  "1,2,3,4,5,6,7,0,1,2"
  "12,34,56,70,12"
  "1,234,567,012"
  "12,3456,7012"
  "12345,67012"
  "1234,567012"
  "123,4567012"
  "12,34567012"
  "1,234567012"
  "1234567012"
  "1234567012")

(deftest format.o.24
  (apply
   #'values
   (loop for i from 0 to 10
	 for args = (make-list i)
	 for s = (apply #'format nil "~,,,#@:O" #o1234567012 args)
	 collect s))
  "+1,2,3,4,5,6,7,0,1,2"
  "+12,34,56,70,12"
  "+1,234,567,012"
  "+12,3456,7012"
  "+12345,67012"
  "+1234,567012"
  "+123,4567012"
  "+12,34567012"
  "+1,234567012"
  "+1234567012"
  "+1234567012")

(def-format-test format.o.25
  "~+10o" (#o1234) "      1234")

(def-format-test format.o.26
  "~+10@O" (#o1234) "     +1234")

(def-format-test format.o.27
  "~-1O" (#o1234) "1234")

(def-format-test format.o.28
  "~-1000000000000000000o" (#o1234) "1234")

(def-format-test format.o.29
  "~vo" ((1- most-negative-fixnum) #o1234) "1234")

;;; Randomized test

(deftest format.o.30
  (loop
   for mincol = (and (coin) (random 50))
   for padchar = (and (coin)
		      (random-from-seq +standard-chars+))
   for commachar = (and (coin)
			(random-from-seq +standard-chars+))
   for commaint = (and (coin) (1+ (random 10)))
   for k = (ash 1 (+ 2 (random 30)))
   for x = (- (random (+ k k)) k)
   for fmt = (concatenate
	      'string
	      (if mincol (format nil "~~~d," mincol) "~,")
	      (if padchar (format nil "'~c," padchar) ",")
	      (if commachar (format nil "'~c," commachar) ",")
	      (if commaint (format nil "~do" commaint) "o"))
   for s1 = (format nil fmt x)
   for s2 = (format nil "~v,v,v,vo" mincol padchar commachar commaint x)
   repeat 2000
   unless (string= s1 s2)
   collect (list mincol padchar commachar commaint fmt x s1 s2))
  nil)
