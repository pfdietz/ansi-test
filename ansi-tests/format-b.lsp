;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Aug  1 05:10:10 2004
;;;; Contains: Tests of the ~B format directive

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(deftest format.b.1
  (with-standard-io-syntax
   (loop for x = (ash 1 (+ 2 (random 80)))
	 for i = (- (random (+ x x)) x)
	 for s1 = (format nil "~B" i)
	 for j = (let ((*read-base* 2)) (read-from-string s1))
	 repeat 1000
	 when (or (/= i j)
		  (find #\+ s1)
		  (loop for c across s1 thereis (not (find c "-01"))))
	 collect (list i s1 j)))
  nil)

(deftest format.b.2
  (with-standard-io-syntax
   (loop for x = (ash 1 (+ 2 (random 80)))
	 for i = (- (random (+ x x)) x)
	 for s1 = (format nil "~@b" i)
	 for j = (let ((*read-base* 2)) (read-from-string s1))
	 repeat 1000
	 when (or (/= i j)
		  (loop for c across s1 thereis (not (find c "-+01"))))
	 collect (list i s1 j)))
  nil)

(deftest format.b.3
  (with-standard-io-syntax
   (loop for x = (ash 1 (+ 2 (random 80)))
	 for mincol = (random 30)
	 for i = (- (random (+ x x)) x)
	 for s1 = (format nil "~b" i)
	 for s2 = (format nil (format nil "~~~db" mincol) i)
	 for pos = (search s1 s2)
	 repeat 1000
	 when (or (null pos)
		  (and (> mincol (length s1))
		       (or (/= (length s2) mincol)
			   (not (eql (position #\Space s2 :test-not #'eql)
				     (- (length s2) (length s1)))))))
	 collect (list i mincol s1 s2 pos)))
  nil)

(deftest format.b.4
  (with-standard-io-syntax
   (loop for x = (ash 1 (+ 2 (random 80)))
	 for mincol = (random 30)
	 for i = (- (random (+ x x)) x)
	 for s1 = (format nil "~@B" i)
	 for s2 = (format nil (format nil "~~~d@b" mincol) i)
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

(deftest format.b.5
  (with-standard-io-syntax
   (loop for x = (ash 1 (+ 2 (random 80)))
	 for mincol = (random 30)
	 for padchar = (random-from-seq +standard-chars+)
	 for i = (- (random (+ x x)) x)
	 for s1 = (format nil "~b" i)
	 for s2 = (format nil (format nil "~~~d,'~c~c" mincol padchar
				      (random-from-seq "bB"))
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

(deftest format.b.6
  (with-standard-io-syntax
   (loop for x = (ash 1 (+ 2 (random 80)))
	 for mincol = (random 30)
	 for padchar = (random-from-seq +standard-chars+)
	 for i = (- (random (+ x x)) x)
	 for s1 = (format nil "~b" i)
	 for s2 = (format nil "~v,vb" mincol padchar i)
	 for pos = (search s1 s2)
	 repeat 1000
	 when (or (null pos)
		  (and (> mincol (length s1))
		       (or (/= (length s2) mincol)
			   (find padchar s2 :end (- (length s2) (length s1))
				 :test-not #'eql))))
	 collect (list i mincol s1 s2 pos)))
  nil)

(deftest format.b.7
  (with-standard-io-syntax
   (loop for x = (ash 1 (+ 2 (random 80)))
	 for mincol = (random 30)
	 for padchar = (random-from-seq +standard-chars+)
	 for i = (- (random (+ x x)) x)
	 for s1 = (format nil "~@B" i)
	 for s2 = (format nil "~v,v@b" mincol padchar i)
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

(deftest format.b.8
  (loop for i from -7 to 7
	for s1 = (format nil "~b" i)
	for s2 = (format nil "~:b" i)
	unless (string= s1 s2)
	collect (list i s1 s2))
  nil)

(deftest format.b.9
  (with-standard-io-syntax
   (loop for x = (ash 1 (+ 2 (random 80)))
	 for i = (- (random (+ x x)) x)
	 for commachar = #\,
	 for s1 = (format nil "~b" i)
	 for s2 = (format nil "~:B" i)
	 repeat 1000
	 unless (and (string= s1 (remove commachar s2))
		     (not (eql (elt s2 0) commachar))
		     (or (>= i 0) (not (eql (elt s2 1) commachar)))
		     (let ((len (length s2))
			   (ci+1 4))
		       (loop for i from (if (< i 0) 2 1) below len
			     always (if (= (mod (- len i) ci+1) 0)
					(eql (elt s2 i) commachar)
				      (find (elt s2 i) "01")))))
	 collect (list x i commachar s1 s2)))
  nil)

(deftest format.b.10
  (with-standard-io-syntax
   (loop for x = (ash 1 (+ 2 (random 80)))
	 for i = (- (random (+ x x)) x)
	 for commachar = (random-from-seq +standard-chars+)
	 for s1 = (format nil "~b" i)
	 for s2 = (format nil "~,,v:b" commachar i)
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

(deftest format.b.11
  (with-standard-io-syntax
   (loop for x = (ash 1 (+ 2 (random 80)))
	 for i = (- (random (+ x x)) x)
	 for commachar = (random-from-seq +standard-chars+)
	 for s1 = (format nil "~b" i)
	 for s2 = (format nil (format nil "~~,,'~c:~c" commachar
				      (random-from-seq "bB"))
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

(deftest format.b.12
  (with-standard-io-syntax
   (loop for x = (ash 1 (+ 2 (random 80)))
	 for i = (- (random (+ x x)) x)
	 for commachar = (random-from-seq +standard-chars+)
	 for commaint = (1+ (random 20))
	 for s1 = (format nil "~b" i)
	 for s2 = (format nil "~,,v,v:B" commachar commaint i)
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

(deftest format.b.13
  (with-standard-io-syntax
   (loop for x = (ash 1 (+ 2 (random 80)))
	 for i = (- (random (+ x x)) x)
	 for commachar = (random-from-seq +standard-chars+)
	 for commaint = (1+ (random 20))
	 for s1 = (format nil "~@B" i)
	 for s2 = (format nil "~,,v,v:@b" commachar commaint i)
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

(deftest format.b.14
  (format nil "~vb" nil #b110100)
  "110100")

(deftest format.b.15
  (format nil "~6,vB" nil #b100)
  "   100")

(deftest format.b.16
  (format nil "~,,v:b" nil #b10011)
  "10,011")

(deftest format.b.17
  (format nil "~,,'*,v:B" nil #b10110)
  "10*110")

;;; When the argument is not an integer, print as if using ~A and base 10

(deftest format.b.18
  (loop for x in *mini-universe*
	for s1 = (format nil "~b" x)
	for s2 = (let ((*print-base* 2)) (format nil "~A" x))
	unless (or (integerp x) (string= s1 s2))
	collect (list x s1 s2))
  nil)

(deftest format.b.19
  (loop for x in *mini-universe*
	for s1 = (format nil "~:B" x)
	for s2 = (let ((*print-base* 2)) (format nil "~A" x))
	unless (or (integerp x) (string= s1 s2))
	collect (list x s1 s2))
  nil)

(deftest format.b.20
  (loop for x in *mini-universe*
	for s1 = (format nil "~@b" x)
	for s2 = (let ((*print-base* 2)) (format nil "~A" x))
	unless (or (integerp x) (string= s1 s2))
	collect (list x s1 s2))
  nil)

(deftest format.b.21
  (loop for x in *mini-universe*
	for s1 = (format nil "~@:B" x)
	for s2 = (let ((*print-base* 2)) (format nil "~A" x))
	unless (or (integerp x) (string= s1 s2))
	collect (list x s1 s2))
  nil)

;;; Must add tests for non-integers when the parameters
;;; are specified, but it's not clear what the meaning is.
;;; Does mincol apply to the ~A equivalent?  What about padchar?
;;; Are comma-char and comma-interval always ignored?

;;; # arguments

(deftest format.b.22
  (apply
   #'values
   (loop for i from 0 to 10
	 for args = (make-list i)
	 for s = (apply #'format nil "~#b" #b11001 args)
	 collect s))
  "11001"
  "11001"
  "11001"
  "11001"
  "11001"
  " 11001"
  "  11001"
  "   11001"
  "    11001"
  "     11001"
  "      11001")

(deftest format.b.23
  (apply
   #'values
   (loop for i from 0 to 10
	 for args = (make-list i)
	 for s = (apply #'format nil "~,,,#:B" #b1100100010 args)
	 collect s))
  "1,1,0,0,1,0,0,0,1,0"
  "11,00,10,00,10"
  "1,100,100,010"
  "11,0010,0010"
  "11001,00010"
  "1100,100010"
  "110,0100010"
  "11,00100010"
  "1,100100010"
  "1100100010"
  "1100100010")

(deftest format.b.24
  (apply
   #'values
   (loop for i from 0 to 10
	 for args = (make-list i)
	 for s = (apply #'format nil "~,,,#@:B" #b1100100010 args)
	 collect s))
  "+1,1,0,0,1,0,0,0,1,0"
  "+11,00,10,00,10"
  "+1,100,100,010"
  "+11,0010,0010"
  "+11001,00010"
  "+1100,100010"
  "+110,0100010"
  "+11,00100010"
  "+1,100100010"
  "+1100100010"
  "+1100100010")

(deftest format.b.25
  (format nil "~+10b" #b1101)
  "      1101")

(deftest format.b.26
  (format nil "~+10@B" #b1101)
  "     +1101")

(deftest format.b.27
  (format nil "~-1b" #b1101)
  "1101")

(deftest format.b.28
  (format nil "~-1000000000000000000B" #b1101)
  "1101")

(deftest format.b.29
  (format nil "~vb" (1- most-negative-fixnum) #b1101)
  "1101")

;;; Randomized test

(deftest format.b.30
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
	      (if commaint (format nil "~db" commaint) "b"))
   for s1 = (format nil fmt x)
   for s2 = (format nil "~v,v,v,vb" mincol padchar commachar commaint x)
   repeat 2000
   unless (string= s1 s2)
   collect (list mincol padchar commachar commaint fmt x s1 s2))
  nil)