;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Sep 20 09:45:15 2003
;;;; Contains: Miscellaneous tests

;;;
;;; This file contains odds-and-ends, mostly tests that came up as
;;; bug-stimulators in various implementations.
;;;

(in-package :cl-test)

(deftest misc.1
  (funcall
   (COMPILE NIL '(LAMBDA (B)
			 (DECLARE (TYPE (INTEGER 8 22337) B))
			 (+ B 2607688420)))
   100)
  2607688520)

(deftest misc.2
  (funcall (compile nil
		    '(lambda (b) (integer-length (dpb b (byte 4 28) -1005))))
	   12800263)
  32)

(deftest misc.3
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (optimize (speed 3) (debug 1)))
       (let ((v7
	      (let ((v2 (block b5 (return-from b5 (if t b -4)))))
		a)))
	 -65667836)))
   1 2)
  -65667836)

(deftest misc.4
  (funcall
   (compile
    nil
    '(lambda (a b c)
	   (declare (type (integer -629491 -333) a)
		    (type (integer -142 1) b)
		    (type (integer 0 12604) c)
		    (optimize (speed 3) (safety 1) (debug 1)))
	   (let ((v6 (block b7 (return-from b7 (if (eql b 0) 1358159 a)))))
		b)))
   -1000 -17 6143)
  -17)

(deftest misc.5
  (funcall
   (compile nil
	    '(lambda () (* 390 (- (signum (logeqv -8005440 -2310))
				  -10604863)))))
  4135896180)

(deftest misc.6
  (funcall
   (compile nil
	    '(lambda (a c)
		     (declare (optimize (speed 3) (debug 1)))
		     (flet ((%f14 () (if c a -486826646)))
			   (let ((v7 (flet ((%f18 () (%f14))) a)))
				(let ((v5 (%f14)))
				     0)))))
   10 20)
  0)

(deftest misc.7
  (funcall (compile nil
		    '(lambda (c) (declare (optimize (speed 3) (debug 1)))
		       (flet ((%f18 () -36))
			 (flet ((%f13 ()
				      (let () (block b8 (return-from b8 c)))))
			   (%f18)))))
	   10)
  -36)

(deftest misc.8
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (optimize (speed 3) (debug 1)))
       (let ((v3 (flet ((%f12 () (min b (block b2 (return-from b2 a)))))
		       a)))
	    (block b7
		   (flet ((%f5 () (return-from b7 b)))
			 (%f5))))))
   10 20)
  20)

(deftest misc.9
  (funcall
   (compile
    nil
    '(lambda ()
	     (declare (optimize (speed 3) (debug 1)))
	     (block b6
		    (flet ((%f3 ()
				(ldb (byte 19 23)
				     (block b1
					    (let ()
						 (- (if nil (return-from b6 89627)
							1160)
						    (return-from b1 22923)))))))
			  1)))))
  1)

(deftest misc.10
  (funcall
   (compile
    nil
    '(lambda (c)
	     (declare (optimize (speed 3) (debug 1))
		      (type (integer -15417757 5816) c))
	     (flet ((%f3 () (if nil -3143 c)))
		   (block b5
			  (let ((v7 (if (< 23613642 (%f3)) c -23097977)))
			       (let ((v5
				      (return-from b5
						   (if (eql c v7)
						       (let ((v6 (%f3))) 4650813)
						       782))))
				    -4362540))))))
   -10000)
  782)

(deftest misc.11
  (funcall
   (compile
    nil
    '(lambda (a b c)
	     (declare (optimize (speed 3) (debug 1)))
	     (block b8
		    (logxor
		     (let ((v3 (return-from b8 120789657))) 3690)
		     (block b2
			    (flet ((%f9 ()
					(flet ((%f10 () -1))
					      c)))
				  (flet ((%f3 () (let () (return-from b2 b))))
					a)))))))
   1 2 3)
  120789657)

(deftest misc.12
  (funcall
   (compile
    nil
    '(lambda (c)
	     (declare (optimize (speed 3) (safety 1) (debug 1))
		      (type (integer -171067 -5) c))
	     (flet ((%f16 ()
			  (flet ((%f12 () 439))
				3358)))
		   (flet ((%f14 () c))
			 (if (%f14) -1 (%f14))))))
   -100)
  -1)

(deftest misc.13
  (funcall
   (compile
    nil
    '(lambda (b c)
	     (declare (optimize (speed 3) (safety 1) (debug 1))
		      (type  (integer -1554410 36086789) b)
		      (type (integer -15033876209 126774299) c)
		      )
	     (block b3
		    (flet ((%f9 ()
				(abs
				 (flet ((%f5 ()
					     (return-from b3 -2)))
				       (if (if (<= 1 c) b (%f5)) -65 -47895812)))))
			  (min
			   (let ((v3 (let ((v8 (%f9))) b))) b)
			   (if (= 1364001 (%f9))
			       (logeqv (block b5 -2713) -247)
			       -19))))))
   0 0)
  -2)

(deftest misc.14
  (funcall
   (compile
    nil
    '(lambda (c)
	     (declare (notinline logandc1))
	     (block b6
		    (flet ((%f17 () (return-from b6 c)))
			  (logandc1 (%f17)
				    (if 1
					450967818
					(let ((v1 (%f17))) -17)))))))
   10)
  10)

(deftest misc.15
  (funcall
   (compile
    nil
    '(lambda (a b)
	     (declare (optimize (speed 3) (safety 1) (debug 1)))
	     (flet ((%f6 () a))
		   (block b5
			  (flet ((%f14 ()
				       (min 17593 (block b1 (return-from b1 b)))))
				(block b7 (if (%f6) (return-from b7 28182012)
					      (return-from b5 0))))))))
   3 5)
  28182012)

(deftest misc.16
  (funcall
   (compile
    nil
    '(lambda (a c)
	     (flet ((%f14 ()
			  (block b6
				 (flet ((%f7 () (return-from b6 4)))
				       (if 587793 (if (%f7) c -23086423) (%f7))))))
		   (block b1
			  (flet ((%f18 () a))
				(logandc1 (return-from b1 -2781)
					  (if (%f14) 58647578 -396746)))))))
   1 2)
  -2781)

(deftest misc.17
  (funcall
   (compile
    nil
    '(lambda (a b c)
	     (declare (optimize (speed 3) (safety 1) (debug 1))
		      (type (integer 4 23363) b)
		      (type (integer -32681 41648) c)
		      )
	     (flet ((%f18 ()
			  (if nil c b)))
		   (if (if (> -71810514 a) 102077 465393)
		       (block b3 (if (%f18) (return-from b3 c) c))
		       (%f18)))))
   0 10 1000)
  1000)

(deftest misc.18
  (funcall
   (compile
    nil
    '(lambda (a b c)
	     (declare (optimize (speed 3) (safety 1) (debug 1))
		      (type (integer 7 58010860) a)
		      (type (integer -3573280 -1) b)
		      (type (integer -920848 -819) c)
		      )
	     (flet ((%f15 () (if (logbitp 5 a) a c)))
		   (min (if (%f15) b -39) (if (> 0 -14756) b (%f15))))))
   8 -1000 -10000)
  -1000)

(deftest misc.19
  (funcall
   (compile
    nil
    '(lambda (a b c)
	     (declare (type (integer 54 3862515) a) (type (integer -961325 1539) b)
		      (type (integer 6 31455) c) (ignorable a b c)
		      (optimize (speed 3) (safety 1) (debug 1)))
	     (lognor
	      (flet ((%f13 () b)) (%f13))
	      (flet ((%f1 () (return-from %f1 a)))
		    (labels ((%f3 () (%f1)))
			    -428)))))
   100 0 200)
  427)

(deftest misc.20
  (funcall
   (compile
    nil
    '(lambda (a b c)
	     (declare (type (integer -1 31880308) a)
		      (type (integer -11374222037 5331202966) b)
		      (type (integer -483 -1) c)
		      (ignorable a b c)
		      (optimize (speed 3) (safety 1) (debug 1)))
	     (labels ((%f6 () a))
		     (if (eql (let ((v9 (%f6))) -50072824) c)
		      28146341
		      (if (< 119937 21304962) 21304962 (%f6))))))
   0 0 -1)
  21304962)

(deftest misc.21
  (funcall
   (compile
    nil
    '(lambda (a b c)
	     (declare (type (integer 398 3955) a) (type (integer 233 464963) b)
		      (type (integer -124477 16) c) (ignorable a b c)
		      (optimize (speed 3) (safety 1) (debug 1)))
	     (logior
	      (flet ((%f18 ()
			   -3584768))
		    (%f18))
	      (flet ((%f1 ()
			  (return-from %f1 c)))
		    (flet ((%f9 ()
				(if (%f1) 24181 7)))
			  56048)))))
   400 300 0)
  -3547152)

(deftest misc.22
  (funcall
   (compile
    nil
    '(lambda (a b c)
	     (declare (type (integer -126378 -103) a)
		      (type (integer -1158604975 1) b)
		      (type (integer 502 28036) c)
		      (ignorable a b c)
		      (optimize (speed 3) (safety 1) (debug 1)))
	     (labels ((%f13 () c))
		     (labels ((%f3 ()
				   (logandc1
				    c
				    (block b6
					   (max -73100
						(if b (return-from b6 4935) (%f13)))))))
			     (%f13)))))
   -200 0 1000)
  1000)

(deftest misc.23
  (funcall
   (compile
    nil
    '(lambda (a b c)
	     (declare (type (integer 1 18911480) a)
		      (type (integer -1 48333) b)
		      (type (integer -3881001767 -1937357) c)
		      (ignorable a b c)
		      (optimize (speed 3) (safety 1) (debug 1)))
	     (labels ((%f10 () c))
		     (block b7
			    (logorc2
			     (* (%f10)
				(if (ldb-test (byte 27 1) -11337)
				    (return-from b7 -2)
				    246137101))
			     (min (%f10) (return-from b7 -76114)))))))
   1 0 -2000000)
  -2)

(deftest misc.24
  (funcall
   (compile
    nil
    '(lambda (a b c)
	     (declare (type (integer -1477249397 -10697252) a)
		      (type (integer -7 54591) b)
		      (type (integer -102559556 15) c)
		      (ignorable a b c)
		      (optimize (speed 3) (safety 1) (debug 1)))
	     (block b8
		    (let ((v1 (return-from b8 a)))
			 (1+
			  (block b3
				 (flet ((%f10 ()
					      (min a (return-from b3 -1))))
				       16776220)))))))
   -11000000 0 0)
  -11000000)

(deftest misc.25
  (funcall
   (compile
    nil
    '(lambda (a b c)
	     (declare (type (integer -944 111244) a)
		      (type (integer 100512 3286178) b)
		      (type (integer -2170236 -107) c)
		      (ignorable a b c)
		      (optimize (speed 3) (safety 1) (debug 1)))
	     (labels ((%f17 () c))
		     (labels ((%f16 () a))
			     (if (if (logbitp 10 1029643) t 355)
				 (if (equal (%f17) b) c a)
				 (if (= 1325844 (%f16)) -50285 (1- (%f17))))))))
   0 200000 -200)
  0)

(deftest misc.26
  (funcall
   (compile
    nil
    '(lambda (c)
	     (declare (optimize speed))
	     (block b5
		    (if (logbitp 6 -97)
			(let ((v2 (block b8 -42484))) c)
			(flet ((%f10 () (return-from b5 -785143)))
			      (let ((v3 (%f10)))
				   (%f10)))))))
   0)
  -785143)

(deftest misc.27
  (funcall
   (compile
    nil
    '(lambda (a b c)
	     (declare (optimize (speed 3) (debug 1)))
	     (labels ((%f14 () c))
		     (logand (%f14)
			     (labels ((%f15 () (logeqv (let ((v1 b)) c)
						       (return-from %f15 -1740))))
				     (labels ((%f8 () (%f15)))
					     a))))))
   5 2 3)
  1)

(deftest misc.28
  (funcall
   (compile
    nil
    '(lambda (a b c)
	  (declare
	   (type (integer 1948 12024) b)
	   (type (integer -104357939 -252) c)
	   (optimize (speed 3) (debug 1)))
    (flet ((%f18 () c))
      (logandc1 (if (eql b (%f18)) 0 a)
                (if (ldb-test (byte 30 30) 1) (%f18) 1)
		))))
   0 2000 -300)
  1)

(deftest misc.29
  (funcall
   (compile
    nil
    '(lambda (a b c)
	     (declare (type (integer 661607 10451683348) a)
		      (type (integer -2 -2) b)
		      (type (integer 5996117 18803237) c)
		      (optimize (speed 3) (safety 1) (debug 1)))
	     (labels ((%f16 () -29))
	       (flet ((%f7 ()
			   (labels ((%f1 () a))
			     (let ()
			       (block b3
				 (if 37101207
				     (return-from b3 -5322045)
				   (let ((v5 b))
				     146099574)))))))
		 (if (%f16) c c)))))
   1000000 -2 6000000)
  6000000)

(deftest misc.30
  (funcall
   (compile
    nil
    '(lambda (c)
	     (declare (type (integer -253 -1) c)
		      (optimize (speed 3) (safety 1) (debug 1)))
	     (flet ((%f8 () c))
		   (if (= (%f8) 481) (%f8) 1779465))))
   -100)
  1779465)

(deftest misc.31
  (funcall
   (compile nil
	    '(lambda () (let ((v9 (labels ((%f13 () nil)) nil)))
			     (let ((v3 (logandc2 97 3)))
				  (* v3 (- 37391897 (logand v3 -66))))))))
  3589619040)

(deftest misc.32
  (funcall
   (compile
    nil
    '(lambda (a d)
	     (declare (type (integer -8507 26755) a)
		      (type (integer -393314538 2084485) d)
		      (optimize (speed 3) (safety 1) (debug 1)))
	     (gcd
	      (if (= 0 a) 10 (abs -1))
	      (logxor -1
		      (min -7580
			   (max (logand a 31365125) d))))))
   1 1)
  1)

(deftest misc.33
  (funcall
   (compile
    nil
    '(lambda (a b c d)
	     (declare (type (integer 240 100434465) a)
		      (optimize (speed 3) (safety 1) (debug 1)))
	     (logxor
	      (if (ldb-test (byte 27 4) d)
		  -1
		  (max 55546856 -431))
	      (logorc2
	       (if (>= 0 b)
		   (if (> b c) (logandc2 c d) (if (> d 224002) 0 d))
		   (signum (logior c b)))
	       (logior a -1)))))
   256 0 0 0)
  55546856)

(deftest misc.34
  (funcall
   (compile nil
	    `(lambda (b c)
		     (declare (type (integer -23228343 2) b)
			      (type (integer -115581022 512244512) c)
			      (optimize (speed 3) (safety 1) (debug 1)))
		     (* (* (logorc2 3 (deposit-field 4667947 (byte 14 26) b))
			   (deposit-field b (byte 25 27) -30424886))
			(dpb b (byte 23 29) c)
			)))
   0 0)
  0)

(deftest misc.35
  (funcall
   (compile
    nil
    '(lambda (c)
	     (declare (type (integer -5945502333 12668542) c)
		      (optimize (speed 3)))
	     (let ((v2 (* c 12)))
		  (- (max (if (/= 109335113 v2) -26479 v2)
			  (deposit-field 311
					 (byte 14 28)
					 (min (max 521326 c) -51)))))))
   12668542)
  26479)

(deftest misc.36
  (funcall
   (compile nil
	    '(lambda ()
	       (declare (notinline + logand)
			(optimize (speed 0)))
	       (logand
		(block b5
		       (flet ((%f1 ()
				   (return-from b5 -220)))
			     (let ((v7 (%f1)))
				  (+ 359749 35728422))))
		-24076))))
  -24284)

(deftest misc.37
  (funcall
   (compile
    nil
    '(lambda (b)
       (declare (notinline -) (optimize (speed 0)))
       (- (block b4
		 (flet ((%f4 ()
			     (return-from b4 b)))
		       (%f4))))))
   10)
  -10)

(deftest misc.38
  (funcall
   (compile
    nil
    '(lambda (x) (declare (type (integer 0 100) x)
			  (optimize (speed 3) (safety 1)))
       (logandc1 x x)))
   79)
  0)

(deftest misc.39
  (funcall
   (compile
    nil
    '(lambda (x) (declare (type (integer 0 100) x)
			  (optimize (speed 3) (safety 1)))
       (logandc2 x x)))
   79)
  0)

(deftest misc.40
  (funcall
   (compile
    nil
    '(lambda (x) (declare (type (integer 0 100) x)
			  (optimize (speed 3) (safety 1)))
       (logorc1 x x)))
   79)
  -1)

(deftest misc.41
  (funcall
   (compile
    nil
    '(lambda (x) (declare (type (integer 0 100) x)
			  (optimize (speed 3) (safety 1)))
       (logorc2 x x)))
   79)
  -1)

(deftest misc.42
  (funcall
   (compile
    nil
    '(lambda (x)
       (declare (type -100 100) x)
       (ldb (byte 1 32) x)))
   -1)
  1)

(deftest misc.43
  (funcall (compile nil
	'(lambda () (flet ((%f2 () 288213285))
		      (+ (%f2) (* 13 (%f2)))))))
  4034985990)
  

(deftest misc.44
  (funcall
   (compile
    nil
    '(lambda (a)
	     (declare (type (integer -917858 964754309) a)
		      (optimize (speed 3)))
	     (* 25 (min (max a 171625820) 171626138))))
   861929141)
  4290653450)

(deftest misc.45
  (funcall
   (compile
    nil
    '(lambda (b)
	     (declare (type (integer 21 9673) b)
		      (optimize (speed 3)))
	     (* (integer-length -198435631) (+ b 137206182))))
   6027)
  3841941852)

(deftest misc.46
  (funcall
   (compile
    nil
    '(LAMBDA (B C)
	     (DECLARE (TYPE (INTEGER 0 1) B) (OPTIMIZE (SPEED 3)))
	     (FLET ((%F2 () (LOGNOR (BLOCK B5 138) C)))
		   (IF (NOT (OR (= -67399 B) B))
		       (DEPOSIT-FIELD (%F2) (BYTE 11 8) -3)
		       C))))
   0 0)
  0)

    