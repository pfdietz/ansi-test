;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Aug  1 07:14:17 2004
;;;; Contains: Tests of the ~f format directive

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

;;; Equivalent to PRIN1 for 0 or (abs x) in range [10^-3,10^7).

(deftest format.f.1
  (let ((*print-readably* nil))
    (loop for x in (remove-duplicates '(0.0s0 0.0f0 0.0d0 0.0l0
				       -0.0s0 -0.0f0 -0.0d0 -0.0l0))
	  for s1 = (format nil "~f" x)
	  for s2 = (prin1-to-string x)
	  unless (string=t s1 s2)
	  collect (list x s1 s2)))
  nil)

(deftest format.f.2
  (let ((*print-readably* nil))
    (loop
     for x = (expt (random-from-seq #(10.s0 10.f0 10.d0 10.0l0))
		   (- (random 10.0s0) 3))
     for s1 = (format nil "~f" x)
     for s2 = (prin1-to-string x)
     repeat 1000
     when (and (<= 1/1000 x)
	       (< x 10000000)
	       (not (string= s1 s2)))
     collect (list x s1 s2)))
  nil)

(deftest format.f.3
  (let ((*print-readably* nil))
    (loop
     for x = (- (expt (random-from-seq #(10.s0 10.f0 10.d0 10.0l0))
		      (- (random 10.0s0) 3)))
     for s1 = (format nil "~f" x)
     for s2 = (prin1-to-string x)
     repeat 1000
     when (and (>= -1/1000 x)
	       (> x -10000000)
	       (not (string= s1 s2)))
     collect (list x s1 s2)))
  nil)

(deftest format.f.4
  (loop for x in (remove-duplicates '(1 1.0s0 1.0f0 1.0d0 1.0l0))
	for s = (format nil "~3f" x)
	unless (string= s "1.0")
	collect (list x s))
  nil)

(deftest format.f.5
  (loop for x in (remove-duplicates '(1 1.0s0 1.0f0 1.0d0 1.0l0))
	for s = (format nil "~2f" x)
	unless (string= s "1.")
	collect (list x s))
  nil)

(deftest format.f.6
  (loop for x in (remove-duplicates '(1 1.0s0 1.0f0 1.0d0 1.0l0))
	for s = (format nil "~4F" x)
	unless (string= s " 1.0")
	collect (list x s))
  nil)

(deftest format.f.7
  (loop for x in (remove-duplicates '(1 1.0s0 1.0f0 1.0d0 1.0l0))
	for s = (format nil "~4@f" x)
	unless (string= s "+1.0")
	collect (list x s))
  nil)

(deftest format.f.8
  (loop for x in (remove-duplicates '(1 1.0s0 1.0f0 1.0d0 1.0l0))
	for s = (format nil "~3@F" x)
	unless (string= s "+1.")
	collect (list x s))
  nil)

(deftest format.f.9
  (loop for x in (remove-duplicates '(1 1.0s0 1.0f0 1.0d0 1.0l0))
	for s = (format nil "~4f" (- x))
	unless (string= s "-1.0")
	collect (list (- x) s))
  nil)

(deftest format.f.10
  (loop for x in (remove-duplicates '(1/2 0.5s0 0.5f0 0.5d0 0.5l0))
	for s = (format nil "~3f" x)
	unless (string= s "0.5")
	collect (list x s))
  nil)

(deftest format.f.11
  (loop for x in (remove-duplicates '(1/2 0.5s0 0.5f0 0.5d0 0.5l0))
	for s = (format nil "~4f" x)
	unless (string= s " 0.5")
	collect (list x s))
  nil)

(deftest format.f.12
  (loop for x in (remove-duplicates '(1/2 0.5s0 0.5f0 0.5d0 0.5l0))
	for s = (format nil "~4,2f" x)
	unless (string= s "0.50")
	collect (list x s))
  nil)

(deftest format.f.13
  (loop for x in (remove-duplicates '(1/2 0.5s0 0.5f0 0.5d0 0.5l0))
	for s = (format nil "~3,2f" x)
	unless (string= s ".50")
	collect (list x s))
  nil)

(deftest format.f.14
  (loop for x in (remove-duplicates '(1/2 0.5s0 0.5f0 0.5d0 0.5l0))
	for s = (format nil "~2,1f" x)
	unless (string= s ".5")
	collect (list x s))
  nil)

(deftest format.f.15
  (loop for x in (remove-duplicates '(1/2 0.5s0 0.5f0 0.5d0 0.5l0))
	for s = (format nil "~4,2@f" x)
	unless (string= s "+.50")
	collect (list x s))
  nil)

(deftest format.f.16
  (loop for x in (remove-duplicates '(1/2 0.5s0 0.5f0 0.5d0 0.5l0))
	for s = (format nil "~2,2f" x)
	unless (string= s ".50")
	collect (list x s))
  nil)

(deftest format.f.17
  (loop for x in (remove-duplicates '(1/2 0.5s0 0.5f0 0.5d0 0.5l0))
	for s = (format nil "~,2f" x)
	unless (string= s "0.50")
	collect (list x s))
  nil)

(deftest format.f.18
  (loop for xn in (remove-duplicates '(1/2 0.5s0 0.5f0 0.5d0 0.5l0))
	for x = (- xn)
	for s = (format nil "~,2f" x)
	unless (string= s "-0.50")
	collect (list x s))
  nil)

(deftest format.f.19
  (loop for x in (remove-duplicates '(5 5.0s0 5.0f0 5.0d0 5.0l0))
	for s = (format nil "~4,2,-1f" x)
	unless (string= s "0.50")
	collect (list x s))
  nil)

(deftest format.f.20
  (loop for x in (remove-duplicates '(1/2 0.5s0 0.5f0 0.5d0 0.5l0))
	for s = (format nil "~4,2,0f" x)
	unless (string= s "0.50")
	collect (list x s))
  nil)

(deftest format.f.21
  (loop for x in (remove-duplicates '(1/20 0.05s0 0.05f0 0.05d0 0.05l0))
	for s = (format nil "~4,2,1f" x)
	unless (string= s "0.50")
	collect (list x s))
  nil)

;;; overflow

(deftest format.f.22
  (loop for x in (remove-duplicates
		  '(1000 1000.0s0 1000.0f0 1000.0d0 1000.0l0))
	for s = (format nil "~5,1,,'*f" x)
	unless (string= s "*****")
	collect (list x s))
  nil)

(deftest format.f.23
  (loop for x in (remove-duplicates
		  '(100 100.0s0 100.0f0 100.0d0 100.0l0))
	for s = (format nil "~5,1,,'*f" x)
	unless (string= s "100.0")
	collect (list x s))
  nil)

(deftest format.f.24
  (loop for x in (remove-duplicates
		  '(100 100.0s0 100.0f0 100.0d0 100.0l0))
	for s = (format nil "~4,0,,'*f" x)
	unless (string= s "100.")
	collect (list x s))
  nil)

(deftest format.f.25
  (loop for x in (remove-duplicates
		  '(100 100.0s0 100.0f0 100.0d0 100.0l0))
	for s = (format nil "~1,1,,f" x)
	unless (string= s "100.0")
	collect (list x s))
  nil)

;;; padchar

(deftest format.f.26
  (loop for x in (remove-duplicates
		  '(100 100.0s0 100.0f0 100.0d0 100.0l0))
	for s = (format nil "~10,1,,f" x)
	unless (string= s "     100.0")
	collect (list x s))
  nil)

(deftest format.f.27
  (loop for x in (remove-duplicates
		  '(100 100.0s0 100.0f0 100.0d0 100.0l0))
	for s = (format nil "~10,1,,,'*f" x)
	unless (string= s "*****100.0")
	collect (list x s))
  nil)

;;; v parameters

(deftest format.f.28
  (loop for x = (random 100.0)
	for s1 = (format nil "~f" x)
	for s2 = (format nil "~vf" nil x)
	repeat 100
	unless (string= s1 s2)
	collect (list x s1 s2))
  nil)

(deftest format.f.29
  (loop for x = (random 100.0)
	for s1 = (format nil "~f" x)
	for s2 = (format nil "~,vf" nil x)
	repeat 100
	unless (string= s1 s2)
	collect (list x s1 s2))
  nil)

(deftest format.f.30
  (loop for x = (random 100.0)
	for s1 = (format nil "~f" x)
	for s2 = (format nil "~,,vf" nil x)
	repeat 100
	unless (string= s1 s2)
	collect (list x s1 s2))
  nil)

(deftest format.f.31
  (loop for x = (random 100.0)
	for s1 = (format nil "~f" x)
	for s2 = (format nil "~,,,vf" nil x)
	repeat 100
	unless (string= s1 s2)
	collect (list x s1 s2))
  nil)

(deftest format.f.32
  (loop for x = (random 100.0)
	for s1 = (format nil "~f" x)
	for s2 = (format nil "~,,,,vf" nil x)
	repeat 100
	unless (string= s1 s2)
	collect (list x s1 s2))
  nil)

;;; Randomized tests

#|
(deftest format.f.33
  (let ((bound (if (> 10000000 most-positive-short-float)
		   most-positive-short-float
		 (coerce 10000000 'short-float))))
    (loop for d = (random 10)
	  for w = (+ 1 d (random 10))
	  for x = (random bound)
	  for xr = (rational x)
	  for s = (format nil "~v,vf" w d x)
	  for sr = (decode-fixed-decimal-string s)
	  for eps = (expt 1/10 d)
	  for abs-xr-sr = (abs (- xr sr))
	  for abs-xr-sr-hi = (abs (- xr (+ sr eps)))
	  for abs-xr-sr-lo = (abs (- xr (- sr eps)))
	  repeat 100
	  unless (and (<= abs-xr-sr abs-xr-sr-hi)
		      (<= abs-xr-sr abs-xr-sr-lo))
	  collect (list d w x xr s sr eps abs-xr-sr abs-xr-sr-hi abs-xr-sr-lo)))
  nil)
|#

(deftest format.f.34
  (with-standard-io-syntax
   (let ((*read-default-float-format* 'short-float))
     (loop for i from (- 1 (ash 1 13)) below (ash 1 13)
	   for sf = (coerce i 'short-float)
	   for s = (format nil "~f" sf)
	   for i2 = (floor (read-from-string s))
	   unless (or (zerop i) (eql i i2))
	   collect (list i sf s i2))))
  nil)

(deftest format.f.35
  (with-standard-io-syntax
   (let ((*read-default-float-format* 'single-float))
     (loop for i = (- (random (1- (ash 1 25))) -1 (ash 1 24))
	   for sf = (coerce i 'single-float)
	   for s = (format nil "~f" sf)
	   for i2 = (floor (read-from-string s))
	   repeat 2000
	   unless (or (zerop i) (eql i i2))
	   collect (list i sf s i2))))
  nil)

(deftest format.f.36
  (with-standard-io-syntax
   (let ((*read-default-float-format* 'double-float))
     (loop for i = (- (random (1- (ash 1 51))) -1 (ash 1 50))
	   for sf = (coerce i 'double-float)
	   for s = (format nil "~f" sf)
	   for i2 = (floor (read-from-string s))
	   repeat 2000
	   unless (or (zerop i) (eql i i2))
	   collect (list i sf s i2))))
  nil)

(deftest format.f.37
  (with-standard-io-syntax
   (let ((*read-default-float-format* 'long-float))
     (loop for i = (- (random (1- (ash 1 51))) -1 (ash 1 50))
	   for sf = (coerce i 'long-float)
	   for s = (format nil "~f" sf)
	   for i2 = (floor (read-from-string s))
	   repeat 2000
	   unless (or (zerop i) (eql i i2))
	   collect (list i sf s i2))))
  nil)

(deftest format.f.38
  (funcall
   (compile
    nil
    '(lambda ()
       (with-standard-io-syntax
	(let ((*read-default-float-format* 'short-float)
	      (total 0)
	      (len 0))
	  (loop for i from (- 1 (ash 1 13)) below (ash 1 13)
		unless (zerop i)
		nconc
		(loop for sf = (coerce i 'short-float)
		      for w = (random 8)
		      for d = (random 4)
		      for s = (format nil "~v,vf" w d sf)
		      for i2 = (ignore-errors (floor (read-from-string s)))
		      repeat 5
		      ; do (print (list w d s i i2))
		      unless (eql i i2)
		      do (incf total)
		      and collect (list i sf w d s i2))
		when (> total 100) collect "count limit exceeded"
		and do (loop-finish)))))))
  nil)

(deftest format.f.39
  (with-standard-io-syntax
   (let ((*read-default-float-format* 'single-float))
     (loop for i = (- (random (1- (ash 1 25))) -1 (ash 1 24))
	   for sf = (coerce i 'single-float)
	   for w = (and (coin) (random 16))
	   for d = (random 4)
	   for s = (format nil "~v,vf" w d sf)
	   for i2 = (floor (read-from-string s))
	   repeat 2000
	   unless (or (zerop i) (eql i i2))
	   collect (list i sf w d s i2))))
  nil)

(deftest format.f.40
  (with-standard-io-syntax
   (let ((*read-default-float-format* 'double-float))
     (loop for i = (- (random (1- (ash 1 51))) -1 (ash 1 50))
	   for sf = (coerce i 'double-float)
	   for w = (and (coin) (random 30))
	   for d = (random 6)
	   for s = (format nil "~v,vf" w d sf)
	   for i2 = (floor (read-from-string s))
	   repeat 2000
	   unless (or (zerop i) (eql i i2))
	   collect (list i sf w d s i2))))
  nil)

(deftest format.f.41
  (with-standard-io-syntax
   (let ((*read-default-float-format* 'long-float))
     (loop for i = (- (random (1- (ash 1 51))) -1 (ash 1 50))
	   for sf = (coerce i 'long-float)
	   for w = (and (coin) (random 30))
	   for d = (random 6)
	   for s = (format nil "~v,vf" w d sf)
	   for i2 = (floor (read-from-string s))
	   repeat 2000
	   unless (or (zerop i) (eql i i2))
	   collect (list i sf w d s i2))))
  nil)

(deftest format.f.42
  (let ((chars +standard-chars+))
    (loop
     for k = (and (coin) (random 6))
     for x = (random (/ (random-from-seq #(#.(coerce (* 32 (1- (ash 1 13))) 'short-float)
					     #.(coerce (* 256 (1- (ash 1 24))) 'single-float)
					     #.(coerce (* 256 (1- (ash 1 50))) 'double-float)
					     #.(coerce (* 256 (1- (ash 1 50))) 'long-float)))
			(if k (expt 10 k) 1)))
     for w = (and (coin) (random 30))
     for d = (and (coin) (random 10))
     for overflowchar = (and (coin) (random-from-seq chars))
     for padchar = (and (coin) (random-from-seq chars))
     for f1 = (concatenate 'string
			   "~"
			   (if w (format nil "~d" w) "")
			   ","
			   (if d (format nil "~d" d) "")
			   ","
			   (if k (format nil "~d" k) "")
			   ","
			   (if overflowchar (format nil "'~c" overflowchar) "")
			   ","
			   (if padchar (format nil "'~c" padchar) "")
			   (string (random-from-seq "fF")))
     for s1 = (format nil f1 x)
     for s2 = (format nil "~v,v,v,v,vf" w d k overflowchar padchar x)
     repeat 2000
     unless (string= s1 s2)
     collect (list x w d k overflowchar padchar f1 s1 s2)))
  nil)

;;; This failed in sbcl 0.8.12.25
(deftest format.f.43
  (format nil "~,,,,',f" 0.0)
  "0.0")

(deftest format.f.44
  (loop for i from 0 below (min #x10000 char-code-limit)
	for x = 2312.9817
	for c = (code-char i)
	for f1 = (and c (format nil "~~,,,,'~cf" c))
	for s1 = (and c (ignore-errors (format nil f1 x)))
	for s2 = (and c (format nil "~,,,,vf" c x))
	unless (equal s1 s2)
	collect (list i c f1 s1 s2))
  nil)
