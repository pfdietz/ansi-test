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
       (declare (type (integer -100 100) x))
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
    '(lambda (b c)
	     (declare (type (integer 0 1) b) (optimize (speed 3)))
	     (flet ((%f2 () (lognor (block b5 138) c)))
		   (if (not (or (= -67399 b) b))
		       (deposit-field (%f2) (byte 11 8) -3)
		       c))))
   0 0)
  0)

(deftest misc.47
  (funcall
   (compile
    nil
    '(lambda (a)
	     (declare (type (integer -4005718822 -50081775) a)
		      (optimize (speed 3) (safety 1) (debug 1)))
	     (lognor (ash a (min 0 a)) a)))
   -2878148992)
  0)

(deftest misc.48
  (funcall
   (compile
    nil
    '(lambda (a) (declare (notinline ash min)) (lognor (ash a (min 0 a)) a)))
   -2878148992)
  0)

(deftest misc.49
  (let ((body '(truncate (logorc1 -996082 C) -2))
	(arg 25337234))
    (values
     (funcall (compile nil `(lambda (c) ,body)) arg)
     (funcall (compile nil `(lambda (c) (declare (notinline truncate))
			      ,body)) arg)))
  -13099001
  -13099001)

(deftest misc.50
  (funcall (compile nil `(lambda (c)
			   (declare (optimize (speed 3))
				    (type (integer 23062188 149459656) c))
			   (mod c (min -2 0))))
	   95019853)
  -1)

(deftest misc.51
  (funcall (compile nil `(lambda (b)
			   (declare (optimize (speed 3))
				    (type (integer 2 152044363) b))
			    (rem b (min -16 0))))
	   108251912)
  8)
  
(deftest misc.53
  (funcall
   (compile nil '(lambda ()
		   (let (x)
		     (block nil
		       (flet ((%f (y z) (if (> y z) (setq x y) (setq x z))))
			 (%f 1 2)
			 (%f (return 14) 2)))
		     x))))
  2)

(deftest misc.54
  (funcall
   (compile nil '(lambda (a c)
		   (declare (type (integer 8 117873977) a)
			    (type (integer -131828754 234037511) c)
			    (optimize (speed 3) (safety 1) (debug 1)))
		   (* (mod (signum a) (max 50 -358301))
		      (* -2320445737132
			 (* (* a (deposit-field a (byte 32 19) a)) c)))))
   11386 165297671)
  -49725654774521915007942373712)

(deftest misc.55
  (funcall
   (compile nil '(lambda (a b c)
		   (declare (type (integer -5498929 389890) a)
			    (type (integer -5029571274946 48793670) b)
			    (type (integer 9221496 260169518304) c)
			    (ignorable a b c)
			    (optimize (speed 3) (safety 1) (debug 1)))
		   (- (mod 1020122 (min -49 -420))
		      (logandc1
		       (block b2
			 (mod c (min -49 (if t (return-from b2 1582) b))))
		       (labels ((%f14 () (mod a (max 76 8))))
			 b)))))
   -1893077 -2965238893954 30902744890)
  2965238894454)

(deftest misc.56
  (funcall
   (compile nil '(lambda (a c)
		   (declare (type (integer -8691408487404 -9) a)
			    (type (integer 266003133 2112105962) c)
			    (optimize (speed 3) (safety 1) (debug 1)))
		   (truncate (max (round a) c) (* (* a a) a))))
   -10 1000)
  -1 0)

(deftest misc.57
  (funcall
   (compile nil '(lambda (a b c)
                         (declare (type (integer -1907 58388940297) a)
                                  (type (integer -646968358 294016) b)
                                  (type (integer -708435313 89383896) c)
                                  (optimize (speed 3) (safety 1) (debug 1)))
                         (let ((v6 (abs (min a (signum c)))))
                              (if (ceiling v6 (max 77 v6)) b 2))))
   50005747335 -363030456 17382819)
  -363030456)

(deftest misc.58
  (funcall
   (compile nil '(lambda (a)
		   (declare (type (integer -23 66141285) a)
			    (optimize (speed 3)))
		   (logorc2 (setq a 35191330) (* a 107))))
   4099241)
  -3764388885)

(deftest misc.59
  (funcall
   (compile nil '(lambda (a b c)
		   (declare (type (integer -3966039360 -879349) a)
			    (type (integer -62642199164 -8993827395) b)
			    (type (integer -8065934654337 223) c)
			    (optimize (speed 3) (safety 1) (debug 1)))
		   (floor (* (ceiling c) c)
			  (max 78 (* b (* a (* a b)))))))
   -1000000 -10000000000 0)
  0 0)

(deftest misc.60
  (funcall
    (compile nil
	     '(lambda ()
		(let ((v5 46660))
		  (setq v5 (signum (rem v5 (max 53 v5))))))))
  0)

(deftest misc.61
  (progn
    (compile nil
	     '(lambda (a b)
		(declare (type (integer -1785799651 -2) a)
			 (type (integer -27 614132331) b)
			 (optimize (speed 3) (safety 1) (debug 1)))
		(ceiling (max (floor -733432 (max 84 -20)) 346)
			 (min -10 (* 17592186028032 (* (* a b) a))))))
    :good)
  :good)

(deftest misc.62
  (funcall (compile nil '(lambda (a)
		(if (and (if a t nil) nil)
		    a
		  (min (block b5 -1) a))))
	   100)
  -1)

(deftest misc.63
  (let* ((form '(flet ((%f12 () (setq c -9868204937)))
		  (if (<= c (%f12)) -2 (if (= c c) b c))))
	 (form1 `(lambda (b c)
		   (declare (type (integer -80421740610 1395590616) c))
		   ,form))
	 (form2 `(lambda (b c) ,form))
	 (vals '(-696742851945 686256271)))
    (eqlt (apply (compile nil form1) vals)
	  (apply (compile nil form2) vals)))
  t)

(deftest misc.64
  (let* ((form '(logcount
		 (if (not (> c (let ((v7 (setq c -246180))) -1)))
		     (ldb (byte 24 11) c)
		   c)))
	 (form1 `(lambda (c)
		   (declare (type (integer -256128 207636) c))
		   ,form))
	 (form2 `(lambda (c) ,form))
	 (vals '(11292))
	 )
    (eqlt (apply (compile nil form1) vals)
	  (apply (compile nil form2) vals)))
  t)

(deftest misc.65
  (let ((form1 '(lambda (b c)
		  (declare (type (integer -350684427436 -255912007) b))
		  (logandc2 c (if (< b (setq b -25647585550)) b 0))))
	(form2 '(lambda (b c)
		  (logandc2 c (if (< b (setq b -25647585550)) b 0))))
	(vals '(-297090677547 -20121092)))
    (eqlt (apply (compile nil form1) vals)
	  (apply (compile nil form2) vals)))
  t)

(deftest misc.66
  (let* ((form '(if (> a (setq a -2198578292))
		    (min b (if (<= a -14866) a -128363))
		  a))
	 (form1 `(lambda (a b)
		   (declare (type (integer -3709231882 0) a))
		   (declare (type (integer -562051054 -1) b))
		   ,form))
	 (form2 `(lambda (a b) ,form))
	 (vals '(-2095414787 -256985442)))
    (eqlt (apply (compile nil form1) vals)
	  (apply (compile nil form2) vals)))
  t)

;;; sbcl/cmucl bug (on sparc)
(deftest misc.67
  (funcall
    (compile nil '(lambda (x)
		    (declare (type (integer 10604862 10604862) x)
			     (optimize speed))
		    (* x 390)))
    10604862)
  4135896180)

;;; cmucl bug (cvs, 10/10/2003)
(deftest misc.68
  (funcall
   (compile nil
	    '(lambda (b)
	       (flet ((%f8 () (rem b (identity (return-from %f8 0)))))
		 (lognor (%f8) 0))))
   0)
  -1)

(deftest misc.69
  (funcall
   (compile nil
	    '(lambda (b)
	       (flet ((%f11 () (logorc2 (block b1 (let () (return-from b1 b)))
					-1984)))
		 b)))
   0)
  0)

(deftest misc.70
  (funcall
   (compile nil '(lambda (c)
		   (declare (type (integer 46156191457 126998564334) c))
		   (truncate c (min -16 186196583))))
   87723029763)
  -5482689360
  3)

(deftest misc.71
  (funcall
   (compile nil
	    '(lambda ()
	       (block b8
		 (if (identity (return-from b8 30))
		     1
		   (identity
		    (block b5
		      (labels ((%f10 () (min -52 (return-from b5 10))))
			20))))))))
  30)

(deftest misc.72
  (funcall
   (compile nil '(lambda ()
		   (flet ((%f13 () (rem 1 (min 0 (return-from %f13 17)))))
		     (%f13)))))
  17)

(deftest misc.73
  (funcall
   (compile nil '(lambda (c)
		   (declare (type (integer 46156191457 126998564334) c))
		   (rem c (min -1 0))))
   87723029763)
  0)

(deftest misc.74
  (funcall (compile nil '(lambda ()
			   (declare (optimize (safety 3) (speed 0) (debug 0)))
			   (ash 6916244 (min 42 -185236061640)))))
  0)

;;; Unwind-protect bug, from sbcl:
;;; "The value NIL is not of type SB-C::NODE."

(deftest misc.75
  (funcall (compile nil '(lambda () (flet ((%f12 () (unwind-protect 1))) 0))))
  0)


;;; cmucl (2003-10-12), "NIL is not of type C::REF"
(deftest misc.76
  (funcall
   (compile nil
	    '(lambda (a c)
		(if nil (unwind-protect (max 521739 (unwind-protect c)))
		  (logandc2 3942 a))))
   0 0)
  3942)

;;; gcl (2003-10-11)  Miscomputation of (mod 0 -53) in compiled code
(deftest misc.77
  (funcall (compile nil '(lambda () (mod 0 -53))))
  0)


;;; cmucl (2003-10-12)  "NIL is not of type C::BYTE-LAMBDA-INFO"
(deftest misc.78
  (funcall
   (compile nil '(lambda ()
		   (declare (optimize (speed 0) (debug 0)))
		   (let ((v4
			  (case 227
			    ((-11113 -106126) (unwind-protect 8473))
			    (t 43916))))
		     -12))))
  -12)

;;; Same as misc.78, but with no declarations
;;; In cmucl (2003-10-12)  "NIL is not of type C::ENVIRONMENT"
(deftest misc.79
  (funcall
   (compile nil '(lambda ()
		   (let ((v4
			  (case 227
			    ((-11113 -106126) (unwind-protect 8473))
			    (t 43916))))
		     -12))))
  -12)

;;; cmucl (2003-10-12) "Invalid number of arguments: 2"
(deftest misc.80
  (funcall
   (compile nil
	    '(lambda (b c)
	       (declare (notinline > logior imagpart))
	       (declare (optimize (speed 0) (debug 0)))
	       (labels ((%f16 ()
			      (imagpart
			       (block b3
				 (logeqv (logior -122516 (if (> -1 0) (return-from b3 c) b))
					 (return-from %f16 32186310))))))
		 (lognor (%f16) b))))
   -123886 -1656)
  57385)

;;; cmucl (2003-10-12) "NIL is not of type C::REF"
(deftest misc.81
  (funcall
   (compile nil '(lambda (b)
		   (block b7
		     (let ((v3 (return-from b7 b)))
		       (unwind-protect b)))))
   17)
  17)

;;; cmucl (2003-10-12) "The assertion C::SUCC failed"
(deftest misc.82
  (funcall
   (compile nil '(lambda (c)
		   (labels ((%f15 ()
				  (* (unwind-protect c)
				     (max -5726369
					  (return-from %f15 3099206)))))
		     c)))
   0)
  0)

;;; cmucl (2003-10-13) "The assertion (NOT (C::BLOCK-DELETE-P BLOCK)) failed."
(deftest misc.83
  (funcall
   (compile nil '(lambda (a c)
		   (flet ((%f8 () (min c (min a (return-from %f8 c)))))
		     c)))
   0 -10)
  -10)

(deftest misc.84
  (funcall
   (compile nil '(lambda (a b)
		   (flet ((%f18 ()
				(let ()
				  (let ()
				    (if (ldb-test (byte 20 23) b) a
				      (return-from %f18 431))))))
		     -674)))
   0 0)
  -674)

(deftest misc.85
  (funcall
   (compile nil
	    '(lambda (c)
	       (labels ((%f14 ()
			      (let ()
				(logandc1 (min -32 (return-from %f14 -69793))
					  c))))
		     156)))
   0)
  156)

;;; Two tests showing bug(s) in clisp (2.31)
(deftest misc.86
  (funcall (compile nil '(lambda (b)
			   (flet ((%f10 nil :bad))
			     (let ((v7 (let ((v2 (%f10))) b)))
			       (unwind-protect b)))))
	   :good)
  :good)

(deftest misc.87
  (apply (compile nil '(lambda (a b c)
			 (let ((v9 a))
			   (let ((v2 (setq v9 c)))
			     (unwind-protect c)))))
	 '(x y z))
  z)

;;; cmucl bug (18e+ 10/15/03)
(deftest misc.88
  (eval '(block b3
	   (max (return-from b3 1)
		(if (unwind-protect (unwind-protect 2)) 3 4))))
  1)

;;; cmucl bug (18e+ 10/15/03)
(deftest misc.89
  (funcall
   (compile nil
	    '(lambda (c)
	       (declare (type (integer 0 130304) c))
	       (- (rem -26 (max 25 (load-time-value 505849129)))
		  (* -15718867961526428520296254978781964 c))))
   0)
  -26)

;;; acl bugs (version 6.2, linux x86 trial)
(deftest misc.90
  (let* ((form '(- 0 (ignore-errors 20763)
		   (logxor b 1 c -7672794) b))
	 (fn1 `(lambda (b c)
		 (declare (type (integer -148895 -46982) b))
		 (declare (type (integer 0 1) c))
		 (declare (optimize (speed 3)))
		 (declare (optimize (safety 1)))
		 ,form))
	 (fn2 `(lambda (b c) ,form)))
    (let ((v1 (funcall (compile nil fn1) -76071 0))
	  (v2 (funcall (compile nil fn2) -76071 0))
	  (v3 (funcall (eval `(function ,fn2)) -76071 0)))
      (if (= v1 v2 v3) :good
	(list v1 v2 v3))))
  :good)

(deftest misc.91
  (let ((fn1 '(lambda ()
		(declare (optimize (speed 3) (safety 1)))
		(ash -10 (min 8 -481))))
	(fn2 '(lambda () (ash -10 (min 8 -481)))))
    (let ((v1 (funcall (compile nil fn1)))
	  (v2 (funcall (compile nil fn2)))
	  (v3 (funcall (eval `(function ,fn2)))))
      (if (= v1 v2 v3)
	  :good
	(list v1 v2 v3))))
  :good)

(deftest misc.92
  (let* ((form '(- -16179207 b (lognor (let () 3) (logxor -17567197 c))))
	 (fn1 `(lambda (b c)
		 (declare (type (integer -621 30) c))
		 (declare (optimize (speed 3)))
		 (declare (optimize (safety 1)))
		 ,form))
	 (fn2 `(lambda (b c) ,form))
	 (vals '(26291532469 -21)))
    (let ((v1 (apply (compile nil fn1) vals))
	  (v2 (apply (compile nil fn2) vals))
	  (v3 (apply (eval `(function ,fn2)) vals)))
      (if (= v1 v2 v3)
	  :good
	(list v1 v2 v3))))
  :good)

(deftest misc.93
  (let* ((form '(ash (1+ (flet ((%f5 (f5-1) c)) c))
		     (min 69 (logxor a b))))
	 (fn1 `(lambda (a b c)
		 (declare (type (integer -128 -109) a)
			  (type (integer -2 -1) b)
			  (optimize (speed 3) (safety 1)))
		 ,form))
	 (fn2 `(lambda (a b c) ,form))
	 (vals '(-123 -1 2590941967601)))
    (eqlt (apply (compile nil fn1) vals)
	  (apply (compile nil fn2) vals)))
  t)

(deftest misc.94
  (not (funcall
	(compile nil '(lambda ()
			(declare (optimize (speed 3) (safety 1) (debug 1)))
			(<= 268435280
			    (load-time-value
			     39763134374436777607194165739302560271120000))))))
  nil)

(deftest misc.95
  (let* ((form '(+ 272 c (if (< b a) -49618 -29042) b))
	 (fn1 `(lambda (a b c)
		 (declare (type (integer -1585918 601848636) a))
		 (declare (type (integer -4 16544323) b))
		 (declare (optimize (speed 3)))
		 (declare (optimize (safety 1)))
		 ,form))
	 (fn2 `(lambda (a b c) ,form))
	 (vals '(601739317 10891850 17452477960)))
    (let ((v1 (apply (compile nil fn1) vals))
	  (v2 (apply (compile nil fn2) vals)))
      (if (eql v1 v2)
	  :good
	(list v1 v2))))
  :good)

(deftest misc.96
  (let* ((form '(max 26 (ceiling b (min -8 (max -1 c)))))
	 (fn1 `(lambda (b c)
		 (declare (type (integer 482134 96074347505) b))
		 (declare (type (integer -4036 -50) c))
		 (declare (optimize (speed 3)))
		 (declare (optimize (safety 1)))
		 ,form))
	 (fn2 `(lambda (b c) ,form))
	 (vals '(90244278480 -338)))
    (let ((v1 (apply (compile nil fn1) vals))
	  (v2 (apply (compile nil fn2) vals)))
      (if (eql v1 v2)
	  :good
	(list v1 v2))))
  :good)

(deftest misc.97
  (let* ((form '(- 349708 (gcd c 0) (logand b b (if (> -8543459 c) 83328 1073))))
	 (fn1 `(lambda (b c)
		 (declare (type (integer 301653 329907) b))
		 (declare (type (integer 171971491 1073721279) c))
		 (declare (optimize (speed 3)))
		 (declare (optimize (safety 1)))
		 ,form))
	 (fn2 `(lambda (b c) ,form))
	 (vals '(321769 1073671227)))
    (let ((v1 (apply (compile nil fn1) vals))
	  (v2 (apply (compile nil fn2) vals)))
      (if (eql v1 v2)
	  :good
	(list v1 v2))))
  :good)

;;; sbcl bugs (0.8.4.40, x86 linux)

(deftest misc.98
  (funcall (compile nil '(lambda (x)
			   (declare (type (integer -1000000 1000000) x))
			   (logand x x 0)))
	   12345)
  0)

(deftest misc.99
  (funcall
   (compile nil '(lambda (a)
                   (declare (type (integer 4303063 101130078) a))
                   (mask-field (byte 18 2) (ash a 77))))
   57132532)
  0)

(deftest misc.100
  (funcall (compile nil '(lambda (c)
			   (declare (type (integer -3924 1001809828) c))
			   (declare (optimize (speed 3)))
			   (min 47 (if (ldb-test (byte 2 14) c)
				       -570344431
				     (ignore-errors -732893970)))))
	   705347625)
  -570344431)

(deftest misc.101
  (funcall
   (compile nil '(lambda (a c)
		   (declare (type (integer 185501219873 303014665162) a))
		   (declare (type (integer -160758 255724) c))
		   (declare (optimize (speed 3)))
		   (let ((v8
			  (- -554046873252388011622614991634432
			     (ignore-errors c)
			     (unwind-protect 2791485))))
		     (max (ignore-errors a)
			  (let ((v6 (- v8 (restart-case 980))))
			    (min v8 v6))))))
   259448422916 173715)
  259448422916)

(deftest misc.102
  (funcall
   (compile nil '(lambda (b)
		   (declare (type (integer -1598566306 2941) b))
		   (declare (optimize (speed 3)))
		   (max -148949 (ignore-errors b))))
   0)
  0)

(deftest misc.103
  (funcall
   (compile nil '(lambda (a b)
		   (min -80
			(abs
			 (ignore-errors
			   (+
			    (logeqv b
				    (block b6
				      (return-from b6
					(load-time-value -6876935))))
			    (if (logbitp 1 a) b (setq a -1522022182249))))))))
   -1802767029877 -12374959963)
  -80)

(deftest misc.104
  (funcall
   (compile nil '(lambda (a) (declare (type (integer 55400028 60748067) a))
		   (lognand 1505 (ash a (let () 40)))))
   58194485)
  -1)

(deftest misc.105
  (funcall
   (compile nil '(lambda (b c)
		   (declare (type (integer -4 -3) c))
		   (block b7
		     (flet ((%f1 (f1-1 f1-2 f1-3)
				 (if (logbitp 0 (return-from b7
						  (- -815145138 f1-2)))
				     (return-from b7 -2611670)
				   99345)))
		       (let ((v2 (%f1 -2464 (%f1 -1146 c c) -2)))
			 b)))))
   2950453607 -4)
  -815145134)

;;; Gives the error The value NIL is not of type INTEGER.  (in sbcl 0.8.4.40)

(deftest misc.106
  (progn
    (eval '(defun misc.106-fn (a b c)
	     (declare (optimize speed))
	     (block b6
	       (flet ((%f8 (f8-1 f8-2) b))
		 (%f8 (%f8 c 338) (if t (return-from b6 a) c))))))
    (misc.106-fn -30271 -1 -3043))
  -30271)

 ;;; "The value NIL is not of type SB-C::IR2-LVAR." (sbcl 0.8.4.40)
(deftest misc.107
  (funcall
   (compile nil
	    '(lambda (b c)
	       (declare (type (integer -29742055786 23602182204) b))
	       (declare (type (integer -7409 -2075) c))
	       (declare (optimize (speed 3)))
	       (floor
		(labels ((%f2 ()
			      (block b6
				(ignore-errors (return-from b6
						 (if (= c 8) b 82674))))))
		  (%f2)))))
   22992834060 -5833)
  82674 0)

;;; cmucl bug (Argument X is not a NUMBER: NIL)

(deftest misc.108
  (funcall
   (compile nil '(lambda (b)
		   (block b7 (- b (ignore-errors (return-from b7 57876))))))
   10)
  57876)

;;; "The assertion (C::CONSTANT-CONTINUATION-P C::CONT) failed." (cmucl)
(deftest misc.109
  (funcall (compile
	    nil
	    '(lambda ()
	       (load-time-value
		(block b4
		  (* (return-from b4 -27)
		     (block b5
		       (return-from b4
			 (return-from b5
			   (ignore-errors (unwind-protect
					      (return-from b5 0))))))))))))
  -27)

;;; This bug was occuring a lot in sbcl, and now occurs in cmucl too
(deftest misc.110
  (funcall
   (compile nil
	    '(lambda (c)
	       (declare (type (integer -1441970837 -427) c))
	       (declare (optimize (speed 3)))
	       (block b7 (abs (min c (ignore-errors (return-from b7 c)))))))
   -500)
  -500)

;;; CLISP (2.31+) compiler bug

(deftest misc.111
  (funcall
   (compile nil
            '(lambda (a c)
               (if (or (ldb-test (byte 12 18) a)
                       (not (and t (not (if (not (and c t)) nil nil)))))
                   170 -110730)))
   3035465333 1919088834)
  170)

;;; sbcl (0.8.5.8) "The value NIL is not of type SB-C::IR2-LVAR."

(deftest misc.112
  (funcall
   (compile nil '(lambda (a)
                   (declare (type (integer -944 -472) a))
                   (declare (optimize (speed 3)))
                   (round
                    (block b3
                      (return-from b3
                        (if (= 55957 a) -117 (ignore-errors
                                               (return-from b3 a))))))))
   -589)
  -589 0)

;;; sbcl (0.8.5.8) "The value NIL is not of type SB-C::CTRAN"

(deftest misc.113
  (funcall
   (compile nil '(lambda (b c)
		   (if (or (ldb-test (byte 8 10) b) t)
		       c
		     (min (if (<= -6467 c) c 6)
			  (flet ((%f3 (f3-1 f3-2)
				      f3-1))
			    (multiple-value-call #'%f3 (values b 107)))))))
   -238 -23658556)
  -23658556)

;;; clisp (1 Oct 2003 cvs HEAD)  "*** - CAR: #:G7744659 is not a LIST"

(deftest misc.114
  (funcall
   (compile nil
            '(lambda (a b)
               (unwind-protect
                   (block b2
                     (flet ((%f1 nil b))
                       (logior (if a
				   (if (ldb-test (byte 23 1) 253966182)
				       (return-from b2 a)
				     -103275090)
				 62410)
                               (if (not (not (if (not nil) t (ldb-test (byte 2 27) 253671809))))
                                   (return-from b2 -22)
                                 (%f1))))))))
   777595384624 -1510893868)
  777595384624)

;;; clisp (1 Oct 2003 cvs HEAD) "Compiler bug!! Occurred in OPTIMIZE-LABEL."

(deftest misc.115
  (funcall
   (compile nil
            '(lambda (a b c)
	       (declare (type (integer 0 1000) a b c))
	       (if (and (if b (not (and (not (or a t)) nil)) nil)
			(logbitp 6 c))
		   c b)))
   0 100 600)
  100)

(deftest misc.116
  (funcall
   (compile nil
	    '(lambda (a c)
	       (declare (type (integer 0 1000) a c))
	       (if (if (and (not (and (not (or a t)) nil)) t) c nil)
		   91 -1725615)))
   0 0)
  91)

(deftest misc.117
  (funcall
   (compile nil
	    '(lambda (a c)
	       (declare (type (integer 0 1000) a c))
	       (if (or c (not (or nil (not (and (not (or a t)) nil)))))
		   373146181 115)))
   0 0)
  373146181)

(deftest misc.118
  (funcall
   (compile nil '(lambda (a)
		   (declare (type (integer 0 10000) a))
		   (if (or (or nil (not (or (not (or a nil)) t))) a) a 9376)))
   0)
  0)

