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
;;; NIL fell through ETYPECASE expression.  Wanted one of (C:FIXUP X86::EA C:TN).
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
  600)

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

(deftest misc.119
  (funcall
   (compile
    nil
    '(lambda ()
       (if (and (if (1+ 0) nil (not (and (not (and (<= 3) nil)) nil)))
		(if (= -31) -20 -2371))
	   1493 39720))))
  39720)

(deftest misc.120
  (funcall
   (compile
    nil
    '(lambda (c)
       (declare (type (integer 377036 4184626) c))
       (if (or (and t (not (and (not (and c nil)) nil))) nil)
	   3470653 c)))
   1000000)
  3470653)

(deftest misc.121
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (if (and (and -92220 (not (and (not (or c nil)) nil))) a) b b)))
   2000000 150000 -1)
  150000)

;;; CAR: #:G243 is not a LIST
(deftest misc.122
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (type (integer 2872749 5754655) a))
       (declare (type (integer 24114340 89504792) b))
       (declare (type (integer 506491 1412971) c))
       (declare (ignorable a b c))
       (declare (optimize (speed 3)))
       (declare (optimize (safety 1)))
       (declare (optimize (debug 1)))
       (- (let ((v7 (ignore-errors a))) -6)
          (logand (if c -13936 c)
                  (block b3 (if (if (or t b) (not nil) c)
                                (return-from b3 -3114)
                              (ignore-errors 7)
                              ))))))
   3000000 30000000 600000)
  15978)

;;; gcl bug (30 Oct 2003)
(deftest misc.123
  (let* ((fn1 '(lambda (b)
		 (declare (optimize (safety 1)))
		 (labels ((%f7 (f7-1 f7-2)
			       (let ((v2 (setq b 723149855)))
				 25620)))
		   (max b
			(multiple-value-call #'%f7 (values b 2))))))
	 (fn2 '(lambda (b)
		 (labels ((%f7 (f7-1 f7-2)
			       (let ((v2 (setq b 723149855)))
				 25620)))
		   (max b
			(multiple-value-call #'%f7 (values b 2))))))
	 (vals '(1439719153))
	 (v1 (apply (compile nil fn1) vals))
	 (v2 (apply (compile nil fn2) vals)))
    (if (eql v1 v2) :good (list v1 v2)))
  :good)

(deftest misc.124
  (let* ((fn1 '(lambda (b)
		 (declare (optimize (safety 1)))
		 (labels ((%f7 (f7-1 f7-2)
			       (let ((v2 (setq b 723149855)))
				 25620)))
		   (max b
			(funcall #'%f7 b 2)))))
	 (fn2 '(lambda (b)
		 (labels ((%f7 (f7-1 f7-2)
			       (let ((v2 (setq b 723149855)))
				 25620)))
		   (max b
			(funcall #'%f7 b 2)))))
	 (vals '(1439719153))
	 (v1 (apply (compile nil fn1) vals))
	 (v2 (apply (compile nil fn2) vals)))
    (if (eql v1 v2) :good (list v1 v2)))
  :good)

;;; This passed in gcl, but I added it for completeness.
(deftest misc.125
  (let* ((fn1 '(lambda (b)
		 (declare (optimize (safety 1)))
		 (labels ((%f7 (f7-1 f7-2)
			       (let ((v2 (setq b 723149855)))
				 25620)))
		   (max b
			(%f7 b 2)))))
	 (fn2 '(lambda (b)
		 (labels ((%f7 (f7-1 f7-2)
			       (let ((v2 (setq b 723149855)))
				 25620)))
		   (max b
			(%f7 b 2)))))
	 (vals '(1439719153))
	 (v1 (apply (compile nil fn1) vals))
	 (v2 (apply (compile nil fn2) vals)))
    (if (eql v1 v2) :good (list v1 v2)))
  :good)


;;; clisp optional argument bug: "SYMBOL-VALUE: 1 is not a SYMBOL"

(deftest misc.126
  (funcall
   (compile
    nil
    '(lambda ()
       (declare (special *should-always-be-true*))
       (labels ((%f10 (f10-1 &optional
                             (f10-2 (cl:handler-bind nil
						     (if *should-always-be-true*
							 (progn 878)
						       (should-never-be-called)
						       )))
                             (f10-3 (cl:handler-case 10)))
                      -15))
         (%f10 -144)))))
  -15)

(deftest misc.127
  (funcall
   (compile
    nil
    '(lambda (a c)
       (flet ((%f10 (f10-1 f10-2) 10))
	 (flet ((%f4
		 (&optional
		  (f4-1 (ldb (byte 10 6)
			     (* 828
				(+ 30 (dpb c (byte 9 30) (%f10 1918433 34107)))
				)))
		  (f4-2 (setq a 0)))
		 2))
	   (%f4 -5)))))
   0 0)
  2)

;;; cmucl (22 Oct 2003 build) bug
;;; The assertion (EQ (C::COMPONENT-KIND C:COMPONENT) :INITIAL) failed.

(deftest misc.128
  (flet ((%f14
	  (f14-1 f14-2
		 &optional
		 (f14-3 (unwind-protect 13059412))
		 (f14-4 452384)
		 (f14-5 -6714))
	  -1))
    (%f14 -2 1 1279896 589726354 -11))
  -1)

(deftest misc.129
  (labels ((%f17 (f17-1 f17-2 &optional (f17-3 (unwind-protect 178)))
		 483633925))
    -661328075)
  -661328075)

(deftest misc.130
  (let* ((fn1
	  '(lambda (a c)
	     (flet ((%f10 (&optional (f10-1 -6489) (f10-2 (+ c)))
			  a))
	       (multiple-value-call #'%f10 (values -178858 a)))))
	 (fn2
	  '(lambda (a c)
	     (declare (notinline values +) (optimize (speed 0) (debug 0)))
	     (flet ((%f10 (&optional (f10-1 -6489) (f10-2 (+ c)))
			  a))
	       (multiple-value-call #'%f10 (values -178858 a)))))
	 (vals '(-13649921 -1813684177409))
	 (v1 (apply (compile nil fn1) vals))
	 (v2 (apply (compile nil fn2) vals)))
    (if (eql v1 v2) :good (list v1 v2)))
  :good)

(deftest misc.131
  (let* ((fn1
	  '(lambda (a b)
	     (max
	      (block b7
		(abs
		 (ignore-errors
		   (if (ldb-test (byte 33 15) (return-from b7 a))
		       b b)))))))
	 (fn2
	  '(lambda (a b)
	     (declare (notinline abs max))
	     (declare (optimize (speed 0)))
	     (declare (optimize (debug 0)))
	     (max
	      (block b7
		(abs
		 (ignore-errors
		   (if (ldb-test (byte 33 15) (return-from b7 a))
		       b b)))))))
	 (vals '(-823894140303 -3))
	 (v1 (apply (compile nil fn1) vals))
	 (v2 (apply (compile nil fn2) vals)))
    (if (eql v1 v2) :good (list v1 v2)))
  :good) 

;;; cmucl (22 Oct 2003)
;;; The assertion (EQ C::ENV
;;;                   (C::LAMBDA-ENVIRONMENT
;;;                      (C::LAMBDA-VAR-HOME C::THING))) failed.

(deftest misc.132
  (funcall
   (compile nil
	    '(lambda (b c)
	       (declare (type (integer -3358662 7782429) b))
	       (declare (type (integer -513018 12740) c))
	       (declare (optimize (speed 3)))
	       (declare (optimize (safety 1)))
	       (declare (optimize (debug 1)))
	       (labels ((%f9
			 (&optional
			  (f9-1
			   (labels
			       ((%f5 (f5-1 f5-2)
				     (floor (ignore-errors f5-1)
					    (min -67 (if (equal -56 c)
							 -11197265 f5-2)))))
			     c))
			  (f9-2 -439518)
			  (f9-3 -2840573))
			 f9-1))
		 (%f9 -193644 b 1368))))
   10 20)
  -193644)

;;; cmucl (22 Oct 2003)  Default for optional parameter is improperly chosen
(deftest misc.133
  (funcall
   (compile nil
	    '(lambda (a b c)
	       (declare (notinline values))
	       (declare (optimize (speed 0) (debug 0)))
	       (flet ((%f15 (&optional (f15-5 c)) f15-5))
		 (multiple-value-call #'%f15 (values -2688612)))))
   1 2 3)
  -2688612)

;;; ACL 6.2 (x86 linux trial) bugs
;;; With optional flet/labels parameters, there's a very high frequency bug
;;; causing the compiler error "Error: `:INFERRED' is not of the expected
;;; type `NUMBER'".  The following tests show this bug.

(deftest misc.134
  (funcall
   (compile nil
	    '(lambda (b)
	       (labels ((%f5 (f5-1 f5-2 f5-3 &optional (f5-4 0)
				   (f5-5
				    (flet ((%f13 (f13-1)
						 (return-from %f13 b))) b)))
			     900654472))
		 183301)))
   13775799184)
  183301)

(deftest misc.135
  (funcall
   (compile nil
	    '(lambda (a b)
	       (labels ((%f4 (&optional (f4-1 (labels ((%f17 nil a)) b)))
			     -14806404))
		 190134)))
   1783745644 268410629)
  190134)

(deftest misc.136
  (funcall
   (compile nil
	    '(lambda (c)
	       (flet ((%f17 (&optional
			     (f17-1 (flet ((%f9 nil c)) 73574919)))
			    643))
		 1039017546)))
   0)
  1039017546)

;;; And these caused segfaults

(deftest misc.137
  (funcall
   (compile nil
	    '(lambda ()
	       (declare (optimize (speed 3)))
	       (declare (optimize (safety 1)))
	       (flet ((%f16 (&optional
			     (f16-2 (lognor -3897747
					    (if nil -1 -127228378))))
			    10))
		 20))))
  20)

(deftest misc.138
  (funcall
   (compile nil
	    '(lambda (c)
	       (declare (type (integer 2996 39280) c))
	       (declare (optimize (speed 3)))
	       (declare (optimize (safety 1)))
	       (if (zerop (labels ((%f8 (&optional
					 (f8-2 (logorc2 c -161957)))
					2176))
			    3))
		   c c)))
   3000)
  3000)

;;; Lispworks 4.2 (x86 linux personal edition) failures


(deftest misc.139
  (let* ((fn1
	  '(lambda (c)
	     (declare (optimize (speed 3)))
	     (logior (labels ((%f1 (f1-1 &optional (f1-2 (setq c 7))) f1-1))
		       (%f1 774 3616592)) c)))
	 (fn2
	  '(lambda (c)
	     (logior (labels ((%f1 (f1-1 &optional (f1-2 (setq c 7))) f1-1))
		       (%f1 774 3616592)) c)))
	 (vals '(-3))
	 (v1 (apply (compile nil fn1) vals))
	 (v2 (apply (compile nil fn2) vals)))
    (if (eql v1 v2) :good (list v1 v2)))
  :good)

(deftest misc.140
  (funcall
   (compile nil
	    '(lambda (a)
	       (ldb (byte 24 20)
		    (labels ((%f12 (&optional (f12-1 149) (f12-2 -3894159)) 34068))
		      (let* ((v4 (%f12))) a)))))
   -1)
  16777215)
 

;;; In Lispworks 4.2 (x86 linux personal edition)
;;; 'Error: *** Ran out of patterns in (MOVE) for (edi NIL)'

(deftest misc.141
  (funcall
   (compile nil
	    '(lambda () (labels ((%f11 (&optional (f11-3 (restart-case 0))) f11-3))
			  (%f11 1)))))
  1)

(deftest misc.142
  (funcall
   (compile nil
	    '(lambda ()
	       (labels ((%f15 (&optional (f15-3 (block b1 (+ 1 (return-from b1 -10)))))
			      f15-3))
		 (%f15)))))
  -10)

;;; cmucl (22 Oct 2003):  NIL is not of type C::REF
(deftest misc.143
  (block b2
      (max (return-from b2 1)
           (let ((v3
                  (unwind-protect
                      (let* ((v1 (ignore-errors -254)))
                        1))))
             -2)))
  1)

;;; (was) The assertion (NOT (C::BLOCK-DELETE-P BLOCK)) failed.
;;; (now) The assertion (NOT (MEMBER C::KIND '(:DELETED :OPTIONAL :TOP-LEVEL))) failed.

(deftest misc.144
  (funcall
   (compile nil
	    '(lambda (a b c)
	       (declare (type (integer 9739325 14941321) c))
	       (labels ((%f7 (f7-1 f7-2 f7-3 &optional (f7-4 b))
			     (return-from %f7 f7-4)))
		 (if (= -76482 c)
		     (if (>= c 10986082) (%f7 a b (%f7 -8088 c -147106 2)) -10502)
		   (%f7 509252 b b)))))
   -200 17 10000000)
  17)

(deftest misc.145
  (funcall
   (compile nil
	    '(lambda (a b c)
	       (declare (optimize (safety 3)))
	       (block b5
		 (return-from b5
		   (logior (if (or c t) b (load-time-value -61)) (return-from b5 -3))))))
   1 2 3)
  -3)

;;; cmucl: order of evaluation error
(deftest misc.146
  (funcall
   (compile nil
	    '(lambda (b)
	       (declare (optimize (speed 3)))
	       (flet ((%f14 (&optional (f14-1 301917227)
				       (f14-2 (setq b 995196571)))
			    f14-1))
		 (%f14 b (block b3 (%f14 -64))))))
   10)
  10)

;;; cmucl (22 Oct 2003): NIL is not of type C::CLEANUP
(deftest misc.147
  (flet ((%f11 () (if nil (ignore-errors -19884254) (unwind-protect -2)))) :good)
  :good)

;;; The assertion (C::CONSTANT-CONTINUATION-P C::CONT) failed.
(deftest misc.148
  (block b2 (logior (return-from b2 484) (restart-case (ignore-errors 1737021))))
  484)

;;; Argument X is not a NUMBER: NIL.
(deftest misc.149
  (funcall
   (compile nil '(lambda (b)
		   (block b1 (- (logand 0 -34 1026491) (ignore-errors (return-from b1 b))))))
   0)
  0)

(deftest misc.149a
  (funcall
   (compile nil '(lambda (a) (block b1 (- a (ignore-errors (return-from b1 1))))))
   0)
  1)

;;; cmucl (11 2003 image)  "NIL is not of type C::CONTINUATION"
(deftest misc.150
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (flet ((%f17
	       (&optional
		(f17-4
		 (labels ((%f13 (f13-1 &optional (f13-2 (multiple-value-prog1 b)))
				-4))
		   (%f13 b (%f13 190)))))
	       -157596))
	 (labels ((%f6 () (%f17))) c))))
   10 20 30000)
  30000)

;;; cmucl (11 2003 x86 linux)  "NIL is not of type C::ENVIRONMENT"
(deftest misc.151
  (funcall
   (compile
    nil
    '(lambda (b c)
       (declare (type (integer -249 97) b))
       (declare (type (integer 3565969 6559088) c))
       (let* ((v7
	       (if (not (= 1030 4))
		   c
		 (logand (if (/= b c) b 34945725) (unwind-protect -12443701)))))
	 5520737)))
   -24 5657943)
  5520737)

;;; sbcl bug (0.8.5.19)
;;; "The value NIL is not of type SB-C::REF."

(deftest misc.152
  (funcall
   (compile nil
	    '(lambda (a)
	       (block b5
		 (let ((v1 (let ((v8 (unwind-protect 9365)))
			     8862008)))
		   (*
		    (return-from b5
		      (labels ((%f11 (f11-1) f11-1))
			(%f11 87246015)))
		    (return-from b5
		      (setq v1
			    (labels ((%f6 (f6-1 f6-2 f6-3) v1))
			      (dpb (unwind-protect a)
				   (byte 18 13)
				   (labels ((%f4 () 27322826))
				     (%f6 -2 -108626545 (%f4))))))))))))
   -6)
  87246015)

(deftest misc.153
  (funcall
   (compile nil
	    '(lambda (a)
	       (if (logbitp 3
			    (case -2
			      ((-96879 -1035 -57680 -106404 -94516 -125088)
			       (unwind-protect 90309179))
			      ((-20811 -86901 -9368 -98520 -71594)
			       (let ((v9 (unwind-protect 136707)))
				 (block b3
				   (setq v9
					 (let ((v4 (return-from b3 v9)))
					   (- (ignore-errors (return-from b3 v4))))))))
			      (t -50)))
		   -20343
		 a)))
   0)
  -20343)

;;; Bug in ecl (cvs head, 4 Nov 2003)
;;; "/tmp/ecl04Coiwc0V.c:48: `lex0' undeclared (first use in this function)"

(deftest misc.154
  (funcall
   (compile nil
	    '(lambda (b)
	       (labels ((%f8 nil -39011))
		 (flet ((%f4 (f4-1 f4-2 &optional (f4-3 (%f8)) (f4-4 b))
			     (%f8)))
		   (%f4 -260093 -75538 -501684 (let ((v9 (%f8))) -3))))))
   0)
  -39011)

;;; "/tmp/ecl1572CbKzu.c:16: too many arguments to function `APPLY'"

(deftest misc.155
  (funcall
   (compile nil
	    '(lambda (a b c)
	       (labels ((%f6 (f6-1 f6-2) c))
		 (multiple-value-call #'%f6 (values a c)))))
   0 10 20)
  20)

;;; "The function C::LDB1 is undefined."

(deftest misc.156
  (funcall
   (compile nil
	    '(lambda ()
	       (let ((v6 (ldb (byte 30 1) 1473))) (let ((v8 v6)) 2395)))))
  2395)

;;; "/tmp/ecl9CEiD1RL5.c:36: `lex0' undeclared (first use in this function)"
   
(deftest misc.157
  (funcall
   (compile nil
	    ' (lambda (c)
		(labels ((%f11 nil 1))
		  (flet ((%f9 (f9-1 f9-2)
			      (case 17466182 ((-12) (%f11)) (t c))))
		    (%f9 -9913 c)))))
   17)
  17)

;;; SBCL (0.8.5.24) bug:  "bogus operands to XOR"

(deftest misc.158
  (funcall
   (compile nil
            '(lambda (a b c)
               (declare (type (integer 79828 2625480458) a))
               (declare (type (integer -4363283 8171697) b))
               (declare (type (integer -301 0) c))
               (if (equal 6392154 (logxor a b))
                   1706
                 (let ((v5 (abs c)))
                   (logand v5
                           (logior (logandc2 c v5)
                                   (common-lisp:handler-case
                                    (ash a (min 36 22477)))))))))
   100000 0 0)
  0)

;;; sbcl (0.8.5.24) The value NIL is not of type SB-C::CTRAN.

(deftest misc.159
  (funcall
   (compile nil
            '(lambda ()
               (let ((v8 70696))
                 (if (equal v8 -536145083)
                     (let ((v2 (setq v8 v8)))
                       (flet ((%f9 (f9-1 f9-2)
                                   309257))
                         (multiple-value-call #'%f9 (values v2 v2))))
                   100)))))
  100)

;;; sbcl (0.8.5.37) The value NIL is not of type SB-C::CTRAN.

(deftest misc.159a
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -105680 2104974) a))
       (declare (type (integer -1881 -1134) b))
       (declare (ignorable a b))
       (declare (optimize (speed 3)))
       (declare (optimize (safety 1)))
       (declare (optimize (debug 1)))
       (block b5
	 (let ((v2
		(if (or (>= 34 a) 108361696)
		    (return-from b5 -1)
		  (lognand b -16023672))))
	   (flet ((%f10
		   (f10-1
		    &optional (f10-2 (if (eql -30 v2) v2 -5)) (f10-3 v2)
		    (f10-4 14))
		   (if (equal a f10-2) f10-4 380663047)))
	     (flet ((%f6 (f6-1 f6-2 f6-3)
			 f6-1))
	       (multiple-value-call #'%f6
				    (values a (%f10 -37243) -47691))))))))
   100 -1200)
  -1)

;;; gcl (9 Nov 2003) bug
;;; Error in FUNCALL [or a callee]: Caught fatal error [memory may be damaged]

(deftest misc.160
  (funcall
   (compile nil
	    '(lambda (c)
	       (declare (notinline + funcall))
	       (+ (labels ((%f1 () -14)) (funcall #'%f1))
		  (flet ((%f2 () (floor c))) (funcall #'%f2)))))
   0)
  -14)

;;; cmucl (9 Nov 2003)
;;; The assertion (NOT (MEMBER C::KIND '(:DELETED :OPTIONAL :TOP-LEVEL))) failed.

(deftest misc.161
  (funcall
   (compile nil
	    '(lambda (a b c)
	       (flet ((%f17 (f17-1 f17-2 f17-3)
			    (flet ((%f2
				    (f2-1 f2-2
					  &optional (f2-3 (return-from %f17 f17-1))
					  (f2-4 (return-from %f17 -57)))
				    b))
			      (multiple-value-call #'%f2 (values c -588 55101157)))))
		 (if nil
		     (let* ((v6 (%f17 102136 3096194 a)))
		       b)
		   c))))
   -511 -2269809964 250738)
  250738)

;;; cmucl (9 Nov 2003) Incorrect result at SPEED 0.

(deftest misc.162
  (let* ((fn `(lambda (a c)
		(declare (notinline funcall)
			 (optimize (speed 0) (debug 0)))
		(labels ((%f17 (f17-1 &optional (f17-4 c))
			       (return-from %f17 (if f17-4 f17-1 49572640))))
		  (funcall #'%f17 15128425 a)))))
    (funcall (compile nil fn) 1 3))
  15128425)

;;; gcl (12 Nov 2003)
;;; C compiler failure during compilation (duplicate case value)

(deftest misc.163
  (funcall
   (compile nil
	    '(lambda (b)
	       (declare (type (integer -15716 3947) b))
	       (case b
		 ((-7 -6 -6) :good)
		 ((-5 -6) :bad)
		 )))

   -6)
  :good)

;;; gcl (13 Nov 2003)
;;; Error in FUNCALL [or a callee]: Caught fatal error [memory may be damaged]

(deftest misc.164
  (funcall
   (compile
    nil
    '(lambda (a)
     (labels ((%f6 (f6-1 f6-2)
		   (cl:handler-case
		    (labels ((%f2 nil (logior a)))
		      (if (eql (%f2) (%f2))
			  2829254 -10723))
		    (error (c) (error c))
		    )))
       (funcall #'%f6 10 20)
       )))
   0)
  2829254)

;;; sbcl failures

;;; The value NIL is not of type SB-C::NODE.
(deftest misc.165
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (block b3
	 (flet ((%f15
		 (f15-1 f15-2 f15-3
			&optional
			(f15-4
			 (flet ((%f17
				 (f17-1 f17-2 f17-3
					&optional (f17-4 185155520) (f17-5 c)
					(f17-6 37))
                                 c))
			   (%f17 -1046 a 1115306 (%f17 b -146330 422) -337817)))
			(f15-5 a) (f15-6 -40))
		 (return-from b3 -16)))
	   (multiple-value-call #'%f15 (values -519354 a 121 c -1905))))))
   0 0 -5)
  -16)

;;; failed AVER:
;;;      "(NOT
;;; (AND (NULL (BLOCK-SUCC B))
;;;      (NOT (BLOCK-DELETE-P B))
;;;      (NOT (EQ B (COMPONENT-HEAD #)))))"

(deftest misc.166
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (labels ((%f4
		 (f4-1 f4-2 &optional (f4-3 b) (f4-4 c) (f4-5 -170))
		 (let ((v2
			(flet ((%f3
				(f3-1
				 &optional (f3-2 28476586) (f3-3 c)
				 (f3-4 -9240))
				(return-from %f4 1)))
			  (multiple-value-call
			   #'%f3
			   (values -479909 19843799 f4-5 -463858)))))
		       b)))
	 c)))
   0 0 -223721124)
  -223721124)

(deftest misc.167
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (flet ((%f5 (f5-1 f5-2)
		   (return-from %f5 604245664)))
	 (flet ((%f12
		 (f12-1 f12-2
			&optional (f12-3 c) (f12-4 -579456)
			(f12-5
			 (labels ((%f9
				   (f9-1
				    &optional
				    (f9-2
				     (%f5 1
					  (let ((v4 (%f5 30732606 a)))
					    b)))
				    (f9-3 -29)
				    (f9-4
				     (block b4
				       (labels ((%f14 ()
						      (labels ((%f18
								(&optional
								 (f18-1
								  (locally
								   592928))
								 (f18-2 -3)
								 (f18-3
								  (return-from
								      b4 a)))
								f18-1))
							(%f18 74214190 a))))
					 (%f14)))))
                                   -1))
			   (flet ((%f17
				   (f17-1 f17-2 &optional (f17-3 -136045032))
                                   -38655))
			     (%f17 43873 -138030706 -1372492)))))
		 (return-from %f12 -15216677)))
	   (%f12 (%f5 b 2329383) a)))))
   1 2 3)
  -15216677)

(deftest misc.168
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (block b3
	 (flet ((%f11
		 (f11-1 f11-2
			&optional
			(f11-3
			 (block b6
			   (labels ((%f11
				     (f11-1
				      &optional (f11-2 c)
				      (f11-3 (return-from b6 -1806)))
                                     (return-from b3 -28432)))
			     (apply #'%f11 (list -114))))))
		 (return-from %f11 f11-2)))
	   (%f11 b
		 c
		 (labels ((%f10
			   (f10-1 f10-2
				  &optional (f10-3 a) (f10-4 (%f11 -3931 170)))
			   -1704759))
		   c))))))
   1 2 3)
  3)

(deftest misc.169
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (if t -21705
	 (flet ((%f15 (f15-1 f15-2)
                      b))
	   (block b4
	     (%f15 -11112264
		   (labels ((%f2
			     (f2-1
			      &optional (f2-2 (if b -5485340 -1534))
			      (f2-3 -6))
			     (return-from b4 f2-1)))
		     (return-from b4
		       (if b (%f2 c -320813) (%f2 b a a))))))))))
   1 2 3)
  -21705)

;;; sbcl (0.8.5.26)
;;; failed AVER: "(FUNCTIONAL-LETLIKE-P CLAMBDA)"

(deftest misc.170
  (funcall
   (compile
    nil
    '(lambda (b)
       (flet ((%f14 (f14-1 f14-2)
                    (if (if (eql b -7) nil nil)
                        (labels ((%f10 (f10-1 f10-2 f10-3)
                                       7466))
                          (return-from %f14
                            (min
                             (multiple-value-call #'%f10 (values 0 492 f14-1))
                             (max 11 f14-1)
                             (multiple-value-call #'%f10
                                                  (values 439171 f14-2 0)))))
                      1)))
         (let ((v6 (%f14 (logcount b) -386283)))
           56211))))
   17)
  56211)

;;; The value NIL is not of type SB-C::NODE.

(deftest misc.171
  (funcall
   (compile
    nil
    '(lambda (b)
       (block b6
	 (flet ((%f11 (f11-1 f11-2 &optional (f11-3 -2369157) (f11-4 409468))
		      (return-from b6 1)))
	   (block b2
	     (flet ((%f10 (f10-1 f10-2
                            &optional (f10-3 (return-from b6 (return-from b6 -3))))
		     -8))
	       (%f10
		(multiple-value-call #'%f11 (values -5945959 1654846427 -22))
		(return-from b2 b)
		(return-from b2 31258361))))))))
   10)
  1)

  
;;;   segmentation violation at #XA4A0B59

(deftest misc.172
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (notinline list apply))
       (declare (optimize (safety 3)))
       (declare (optimize (speed 0)))
       (declare (optimize (debug 0)))
       (labels ((%f12 (f12-1 f12-2)
		      (labels ((%f2 (f2-1 f2-2)
				    (flet ((%f6 ()
						(flet ((%f18
							(f18-1
							 &optional (f18-2 a)
							 (f18-3 -207465075)
							 (f18-4 a))
							(return-from %f12 b)))
						  (%f18 -3489553
							-7
							(%f18 (%f18 150 -64 f12-1)
							      (%f18 (%f18 -8531)
								    11410)
							      b)
							56362666))))
				      (labels ((%f7
						(f7-1 f7-2
						      &optional (f7-3 (%f6)))
						7767415))
					f12-1))))
			(%f2 b -36582571))))
	 (apply #'%f12 (list 774 -4413)))))
   0 1 2)
  774)

;;; In sbcl 0.8.5.37
;;; "Unreachable code is found or flow graph is not properly depth-first ordered."

(deftest misc.173
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (notinline values))
       (declare (optimize (safety 3)))
       (declare (optimize (speed 0)))
       (declare (optimize (debug 0)))
       (flet ((%f11
	       (f11-1 f11-2
		      &optional (f11-3 c) (f11-4 7947114)
		      (f11-5
		       (flet ((%f3 (f3-1 &optional (f3-2 b) (f3-3 5529))
				   8134))
			 (multiple-value-call #'%f3
					      (values (%f3 -30637724 b) c)))))
	       (setq c 555910)))
	 (if (and nil (%f11 a a))
	     (if (%f11 a 421778 4030 1)
		 (labels ((%f7
			   (f7-1 f7-2
                                 &optional
                                 (f7-3
                                  (%f11 -79192293
                                        (%f11 c a c -4 214720)
                                        b
                                        b
                                        (%f11 b 985)))
                                 (f7-4 a))
			   b))
		   (%f11 c b -25644))
	       54)
	   -32326608))))
   1 2 3)
  -32326608)

;;; In sbcl 0.8.5.37
;;; The value NIL is not of type SB-C:COMPONENT.

(deftest misc.174
  (funcall
   (compile
    nil
    '(lambda (a b c)
         (declare (type (integer 10292971433 14459537906) b))
         (declare (optimize (speed 3)))
         (declare (optimize (safety 1)))
         (declare (optimize (debug 1)))
         (if
          (and (and (/= -51885 b) nil)
               (case (1+ b)
                 ((4 4 3 -4)
                  (let* ((v1
                          (flet ((%f16 (f16-1)
                                   -1858366))
                            (apply #'%f16 b (list)))))
                    -1602321))
                 (t 3)))
          19
          c)))
   0 11000000000 0)
  0)

(deftest misc.174a
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer 23 365478242977) a))
       (declare (type (integer -38847 268231) b))
       (declare (optimize (speed 3)))
       (declare (optimize (safety 1)))
       (declare (optimize (debug 1)))
       (if (not (if (/= b 7) t (not (not a))))
	   (case (setq b -5880)
	     ((8382 3401 2058 39167 62228)
	      (flet ((%f7 (f7-1 f7-2 f7-3) f7-1))
		(multiple-value-call #'%f7 (values -135629 a -410168200))))
	     (t a))
	 15173)))
   30 0)
  15173)

(deftest misc.174b
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -8688 2170) a))
       (declare (type (integer -9938931470 1964967743) b))
       (declare (optimize (speed 3)))
       (declare (optimize (safety 1)))
       (declare (optimize (debug 1)))
       (if
	   (and
	    (if (if (equal b 9) nil t)
		nil
	      (not
	       (logbitp 5
			(labels ((%f5 (f5-1 f5-2 f5-3)
				      4057223))
			  (let ((v9 (%f5 -42 -27504 45026809)))
			    15011)))))
	    (if
		(or a
		    (labels ((%f16 (f16-1)
				   61))
		      (apply #'%f16 275 (list))))
		a
	      t))
	   (setq a -4803)
	 (rem a (max 47 b)))))
   0 0)
  0)

;;; In sbcl 0.8.5.37
;;; "Unreachable code is found or flow graph is not properly depth-first ordered."

(deftest misc.175
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (notinline list apply values signum funcall))
       (declare (optimize (safety 3)))
       (declare (optimize (speed 0)))
       (declare (optimize (debug 0)))
       (labels ((%f4 (f4-1 f4-2 f4-3)
		     (labels ((%f1 (f1-1 f1-2 f1-3)
				   2))
		       (labels ((%f11
				 (f11-1
				  &optional
				  (f11-2
				   (return-from %f4
				     (labels ((%f8
					       (f8-1 f8-2 f8-3
                                                     &optional (f8-4 -35)
                                                     (f8-5 f4-2))
					       f4-1))
				       (funcall #'%f8 53 b f4-1)))))
                                 (return-from %f4 a)))
			 (signum
			  (let ((v4
				 (flet ((%f8
					 (f8-1 f8-2 f8-3
					       &optional (f8-4 b) (f8-5 -560367))
                                         f8-4))
				   (%f8 -27 35395 c -69))))
			    (%f11
			     (multiple-value-call #'%f11
						  (values (%f1 (%f11 b (%f11 v4 f4-3)) f4-3 77936)
							  1628490976))
			     (return-from %f4 (%f1 -9432 f4-1 f4-1)))))))))
	 (flet ((%f7 (f7-1 f7-2 f7-3)
		     (%f4 b f7-3 f7-3)))
	   (flet ((%f14 (f14-1)
			(apply #'%f7 -252 -56169265 -7322946 (list))))
	     (%f14 a))))))
   -70313091 577425217 28052774417)
  -70313091)

(deftest misc.175a
  (funcall
   (compile
    nil
    '(lambda (a b)
         (declare (notinline values list apply logior))
         (declare (optimize (safety 3)))
         (declare (optimize (speed 0)))
         (declare (optimize (debug 0)))
         (if nil
             (logior (flet ((%f5 (f5-1) b)) (%f5 56288))
              (flet ((%f17 (f17-1 f17-2
                          &optional
                          (f17-3 (let () 6857))
                          (f17-4
                           (labels ((%f3 (f3-1 f3-2 f3-3  &optional (f3-4 a) (f3-5 877))
					 139))
                             (apply #'%f3 (list -33052082 b a 1572)))))
			   b))
                (multiple-value-call #'%f17 (values 31 b a b))))
             392)))
   0 0)
  392)

(deftest misc.175b
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -1185422977 2286472818) a))
       (declare (type (integer -211381289038 74868) b))
       (declare (ignorable a b))
       (declare (optimize (speed 3)))
       (declare (optimize (safety 1)))
       (declare (optimize (debug 1)))
       (lognor (unwind-protect -1248)
	       (flet ((%f7
		       (&optional
			(f7-1
			 (flet ((%f1 (f1-1 f1-2 f1-3) 121426))
			   (%f1 b 2337452 (%f1 61767 b a))))
			(f7-2
			 (block b8
			   (logandc1
			    (labels ((%f10 (f10-1 f10-2 f10-3) 323734600))
			      (%f10 (%f10 323734600 323734600 -10165)
				    -607741 (ignore-errors 971588)))
			    (if (>= b -27) (return-from b8 -2)
			      (ignore-errors 237138926))))))
		       f7-2))
		 (apply #'%f7 (list 761316125 b))))))
   1792769319 -60202244870)
  5)


;;; sbcl 0.8.5.37
;;; failed AVER: "(FUNCTIONAL-LETLIKE-P CLAMBDA)"

(deftest misc.176
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (type (integer 162180298 184143783) a))
       (declare (type (integer 702599480988 725878356286) b))
       (declare (type (integer 168 80719238530) c))
       (declare (optimize (speed 3)))
       (declare (optimize (safety 1)))
       (declare (optimize (debug 1)))
       (block b6
	 (flet ((%f10 (f10-1 f10-2)
		      (labels ((%f6 (f6-1 f6-2)
				    f6-1))
			(let ((v2
			       (flet ((%f1 (f1-1 f1-2 f1-3)
					   f1-3))
				 (let ((v8
					(%f1 -11350578
					     (%f6 10414199 13)
					     -58931837)))
				   -239755))))
			  323))))
	   (labels ((%f4
		     (f4-1
		      &optional (f4-2 204) (f4-3 -1)
		      (f4-4
		       (flet ((%f2 (f2-1)
				   (if t (return-from b6 c) a)))
			 (logorc2 (multiple-value-call #'%f2 (values 1))
				  (let* ((v5 (floor (%f2 -1260))))
				    (case (abs (logxor 185664 a))
				      ((-2 5975)
				       (if (or (< b v5) nil)
					   (return-from b6
					     (let ((v10 (%f2 c)))
					       0))
					 (multiple-value-call #'%f10
							      (values -3 a))))
				      (t b)))))))
		     1503938))
	     (multiple-value-call #'%f4 (values -1 a 1853966)))))))
   173549795 725346738048 993243799)
  993243799)

;;; different results (sbcl 0.8.5.37)
;;; May be that setq side effects bug again?

(deftest misc.177
  (let* ((form '(flet ((%f11
			(f11-1 f11-2)
			(labels ((%f4 () (round 200048 (max 99 c))))
			  (logand
			   f11-1
			   (labels ((%f3 (f3-1) -162967612))
			     (%f3 (let* ((v8 (%f4)))
				    (setq f11-1 (%f4)))))))))
		  (%f11 -120429363 (%f11 62362 b))))
	 (vars '(a b c))
	 (vals '(6714367 9645616 -637681868))
	 (fn1 `(lambda ,vars
		  (declare (type (integer 804561 7640697) a))
		  (declare (type (integer -1 10441401) b))
		  (declare (type (integer -864634669 55189745) c))
		  (declare (ignorable a b c))
		  (declare (optimize (speed 3)))
		  (declare (optimize (safety 1)))
		  (declare (optimize (debug 1)))
		  ,form))
	 (fn2 `(lambda ,vars
		 (declare (notinline list apply logand max round))
		 (declare (optimize (safety 3)))
		 (declare (optimize (speed 0)))
		 (declare (optimize (debug 0)))
		 ,form))
	 (compiled-fn1 (compile nil fn1))
	 (compiled-fn2 (compile nil fn2))
	 (results1 (multiple-value-list (apply compiled-fn1 vals)))
	 (results2 (multiple-value-list (apply compiled-fn2 vals))))
    (if (equal results1 results2)
	:good
      (values results1 results2)))
  :good)

;;; sbcl 0.8.5.37
;;; The value NIL is not of type INTEGER.

(deftest misc.178
  (funcall
   (compile
    nil
    '(lambda (a b c)
         (declare (ignorable a b c))
         (declare (optimize (speed 3)))
         (declare (optimize (safety 1)))
         (declare (optimize (debug 1)))
         (let ((v9
                (flet ((%f9
                           (f9-1 f9-2 f9-3
                            &optional (f9-4 -40538)
                            (f9-5
                             (flet ((%f10 (f10-1 f10-2)
                                      (labels ((%f11 (f11-1 f11-2)
                                                 (labels ((%f10 (f10-1 f10-2)
                                                            -1422))
                                                   (if
                                                    (< b
                                                       (%f10
                                                        (%f10 28262437 95387)
                                                        f10-2))
                                                    -1562
                                                    f10-2))))
                                        (let* ((v6 (%f11 59 b)))
                                          (return-from %f10
                                            (apply #'%f11
                                                   f10-1
                                                   (list
                                                    (return-from %f10
                                                      2029647))))))))
                               (apply #'%f10 -3067 3854883 (list)))))
                         64066))
                  (%f9 a 2774 0 c))))
           (flet ((%f18 (f18-1 f18-2 &optional (f18-3 66) (f18-4 b))
                    -6939342))
             (%f18 1274880 (%f18 b a 46746370 -1))))))
   0 0 0)
  -6939342)

;;; sbcl 0.8.5.37
;;; failed AVER: "(FUNCTIONAL-LETLIKE-P CLAMBDA)"

(deftest misc.179
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer 1135 16722) a))
       (declare (type (integer -640723637053 -9049) b))
       (declare (optimize (speed 3)))
       (declare (optimize (safety 1)))
       (declare (optimize (debug 1)))
       (block b3
	 (return-from b3
	   (flet ((%f17 (f17-1 &optional (f17-2 b) (f17-3 b))
			(+ (if t (return-from b3 -64796) a))))
	     (case (%f17 -3908648 -7026139 a)
	       ((41771 -113272 -48004 -39699 50691 -13222)
		(multiple-value-call #'%f17 (values -1963404294 -105)))
	       (t -7026139)))))))
   2000 -10000)
  -64796)

(deftest misc.180
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer 41 484) a))
       (declare (type (integer -2546947 1008697961708) b))
       (declare (optimize (speed 3)))
       (declare (optimize (safety 1)))
       (declare (optimize (debug 1)))
       (if (and (ldb-test (byte 30 10) b) nil)
	   (labels ((%f7 (f7-1 f7-2 &optional (f7-3 -508405733))
                         390004056))
	     (let* ((v4 (multiple-value-call #'%f7 (values b (%f7 b b)))))
	       (multiple-value-call #'%f7
				    (values (%f7 80199 a)
					    (%f7
					     (%f7 a
						  (let* ((v6 (%f7 -226 250)))
						    a))
					     (abs (ceiling v4)))))))
	 -6001)))
   50 0)
  -6001)

;;; sbcl 0.8.5.37
;;; The value NIL is not of type SB-C::TAIL-SET.

(deftest misc.181
  (funcall
   (compile
    nil
    '(lambda (a b)
         (declare (type (integer -74233251043 -16478648860) a))
         (declare (type (integer 0 960962) b))
         (declare (optimize (speed 3)))
         (declare (optimize (safety 1)))
         (declare (optimize (debug 1)))
         (flet ((%f14 ()
                  (if 1
                      (return-from %f14 a)
                      (labels ((%f10 (f10-1 f10-2 f10-3
					    &optional (f10-4 (let* ((v7 a)) 915)))
                                 -1268205049))
                        (labels ((%f18 (f18-1)
                                   (multiple-value-call #'%f10
                                     (values f18-1
                                             (%f10
                                              (%f10 -1495
                                                    (%f10 -384
                                                          -84
                                                          (%f10 -1
                                                                48052
                                                                58909027
                                                                -35812)
                                                          -114)
                                                    (%f10 -391646964
                                                          -28131299
                                                          f18-1
                                                          (%f10 b 368193 a)))
                                              (%f10 f18-1
                                                    -1415811
                                                    f18-1
                                                    267932407)
                                              174)
                                             -58
                                             320))))
                          (let* ((v3 (let ((v7 (return-from %f14 (%f18 -418731))))
				       (%f10 104871 -1196 -21 a))))
                            (labels ((%f1 () (%f18 (%f18 -794761))))
                              (return-from %f14 b))))))))
           (if (%f14) b 887481))))
   -51967629256 809047)
  809047)

(deftest misc.181a
  (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -982285129 -90) a))
       (declare (type (integer 1 82987) b))
       (declare (optimize (speed 3)))
       (declare (optimize (safety 1)))
       (declare (optimize (debug 1)))
       (labels ((%f1 (f1-1 &optional (f1-2 -34) (f1-3 3318057) (f1-4 117))
		     (let ((v9 (let ((v9 (if t (return-from %f1 f1-2) 606042)))
				 f1-1)))
		       (flet ((%f16 (f16-1 f16-2)  292))
			 (labels ((%f2 (f2-1 f2-2 f2-3
					     &optional (f2-4 f1-3) (f2-5 f1-4)
					     (f2-6 -418207187))
				       (%f16 2099 (%f16 f1-2 1157))))
			   (return-from %f1 (%f2 f1-4 -12066040 v9 122107)))))))
	 (flet ((%f5
		 (f5-1
		  &optional
		  (f5-2 (labels ((%f13 (f13-1 f13-2 f13-3
					      &optional (f13-4 a) (f13-5 b))
				       1054213657))
			  (%f13 b 166441 -3)))
		  (f5-3 20102220)
		  (f5-4 (labels ((%f11 (f11-1 f11-2 f11-3)
				       (%f1 -110148 (%f1 -12336576 f11-1 -61))))
			  (let ((v1 (apply #'%f11 -29706 a b (list))))
			    a))))
		 b))
	   (labels ((%f17 (f17-1 f17-2 f17-3
				 &optional (f17-4 -107566292) (f17-5 63) (f17-6 -2))
			  105656))
	     (%f5
	      (%f17 185703492 a a -511
		    (%f1 b b -218142
			 (%f17 -240978 2923208 22 (%f5 1542 68917407 a) b)))
	      -2018
	      -1))))))
   -100 1)
  1)

;;; sbcl 0.8.5.40
;;; Different results from exprs containing ROUND

(deftest misc.182
  (let* ((form '(labels ((%f14 (f14-1 f14-2)
			       (labels ((%f16
					 (f16-1 f16-2
						&optional
						(f16-3 (setq f14-1 (ash f14-1 (min 77 b)))))
					 (logandc2 c -100)))
				 (return-from %f14 (* 2 (gcd f14-1 (%f16 c f14-1)))))))
		  (round (%f14 c c)
			 (max 83 (%f14 (multiple-value-call #'%f14 (values 0 2)) 0)))))
	 (fn1 `(lambda (a b c)
		 (declare (type (integer 5628 8762) a))
		 (declare (type (integer 778 33310188747) b))
		 (declare (type (integer -6699 4554) c))
		 (declare (optimize (speed 3)))
		 (declare (optimize (safety 1)))
		 (declare (optimize (debug 1)))
		 ,form))
	 (fn2 `(lambda (a b c)
		 (declare (notinline values max round gcd * logandc2 min ash))
		 (declare (optimize (safety 3)))
		 (declare (optimize (speed 0)))
		 (declare (optimize (debug 0)))
		 ,form))
	 (vals '(7395 1602862793 -2384))
	 (cfn1 (compile nil fn1))
	 (cfn2 (compile nil fn2))
	 (result1 (multiple-value-list (apply cfn1 vals)))
	 (result2 (multiple-value-list (apply cfn2 vals))))
    (if (equal result1 result2)
	:good
      (values result1 result2)))
  :good)

;;; sbcl 0.8.5.42
;;; failed AVER: "(NOT POPPING)"
;;; Also occurs in cmucl (11/2003 snapshot)

(deftest misc.183
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (type (integer -368154 377964) a))
       (declare (type (integer 5044 14959) b))
       (declare (type (integer -184859815 -8066427) c))
       (declare (ignorable a b c))
       (declare (optimize (speed 3)))
       (declare (optimize (safety 1)))
       (declare (optimize (debug 1)))
       (block b7
	 (flet ((%f3 (f3-1 f3-2 f3-3) 0))
	   (apply #'%f3 0 (catch 'foo (return-from b7 (%f3 0 b c))) c nil)))))
   0 6000 -9000000)
  0)

;;; sbcl 0.8.5.42
;;; failed AVER: "(FUNCTIONAL-LETLIKE-P CLAMBDA)"

(deftest misc.184
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (type (integer 867934833 3293695878) a))
       (declare (type (integer -82111 1776797) b))
       (declare (type (integer -1432413516 54121964) c))
       (declare (optimize (speed 3)))
       (declare (optimize (safety 1)))
       (declare (optimize (debug 1)))
       (if nil
	   (flet ((%f15 (f15-1 &optional (f15-2 c))
			(labels ((%f1 (f1-1 f1-2) 0))
			  (%f1 a 0))))
	     (flet ((%f4 ()
			 (multiple-value-call #'%f15
					      (values (%f15 c 0) (%f15 0)))))
	       (if nil (%f4)
		 (flet ((%f8 (f8-1 &optional (f8-2 (%f4)) (f8-3 0))
			     f8-3))
		   0))))
	 0)))
   3040851270 1664281 -1340106197)
  0)

;;; sbcl 0.8.5.42
;;; invalid number of arguments: 1
;;; ("XEP for LABELS CL-TEST::%F10" ...

(deftest misc.185
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (type (integer 5 155656586618) a))
       (declare (type (integer -15492 196529) b))
       (declare (type (integer 7 10) c))
       (declare (optimize (speed 3)))
       (declare (optimize (safety 1)))
       (declare (optimize (debug 1)))
       (flet ((%f3
	       (f3-1 f3-2 f3-3
                     &optional (f3-4 a) (f3-5 0)
                     (f3-6
                      (labels ((%f10 (f10-1 f10-2 f10-3)
				     0))
                        (apply #'%f10
                               0
                               a
                               (- (if (equal a b) b (%f10 c a 0))
                                  (catch 'ct2 (throw 'ct2 c)))
                               nil))))
	       0))
	 (%f3 (%f3 (%f3 b 0 0 0) a 0) a b b b c))))
   5 0 7)
  0)

(deftest misc.185a
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (type (integer -1304066 1995764) a))
       (declare (type (integer -52262604195 5419515202) b))
       (declare (type (integer -13 94521) c))
       (declare (optimize (speed 3)))
       (declare (optimize (safety 1)))
       (declare (optimize (debug 1)))
       (flet ((%f13 (f13-1 f13-2 f13-3)
		    0))
	 (apply #'%f13
		(%f13 b 0 0)
		(catch 'ct1 0)
		(catch 'ct2 (throw 'ct2 c))
		nil))))
   0 0 0)
  0)

;;; sbcl 0.8.5.42
;;; Different results

(deftest misc.186
  (let* ((form '(labels ((%f3 (f3-1 f3-2) f3-1))
		  (apply #'%f3 b (catch 'ct8 (throw 'ct8 (logeqv (%f3 c 0)))) nil)))
	 (vars '(b c))
	 (fn1 `(lambda ,vars
		 (declare (type (integer -2 19) b)
			  (type (integer -1520 218978) c)
			  (optimize (speed 3) (safety 1) (debug 1)))
		 ,form))
	 (fn2 `(lambda ,vars
		 (declare (notinline logeqv apply)
			  (optimize (safety 3) (speed 0) (debug 0)))
		 ,form))
	 (cf1 (compile nil fn1))
	 (cf2 (compile nil fn2))
	 (result1 (multiple-value-list (funcall cf1 2 18886)))
	 (result2 (multiple-value-list (funcall cf2 2 18886))))
    (if (equal result1 result2)
	:good
      (values result1 result2)))
  :good)

;;; cmucl (11/2003 snapshot)
;;; The assertion (NOT (EQ (C::FUNCTIONAL-KIND C::LEAF) :ESCAPE)) failed.

(deftest misc.187
  (apply
   (eval '(function
	   (lambda (a b c)
	     (declare (notinline))
	     (declare (optimize (safety 3)))
	     (declare (optimize (speed 0)))
	     (declare (optimize (debug 0)))
	     (flet ((%f7 (&optional (f7-1 (catch (quote ct7) 0)) (f7-2 0))
			 c))
	       (let ((v8
		      (flet ((%f14 (f14-1 &optional (f14-2 (%f7 b)))
				   0))
			0)))
		 (%f7 b))))))
   '(2374299 70496 -6321798384))
  -6321798384)

;;; ecl bug
;;; Segmentation violation

(deftest misc.188
  (funcall
   (compile
    nil
    '(lambda (a b c)
          (declare (notinline floor min funcall))
          (declare (optimize (safety 3) (speed 0) (debug 0)))
          (floor (flet ((%f10 (f10-1 f10-2) b)) (%f10 (%f10 0 0) a))
                 (min -37
                      (labels ((%f6 (f6-1 f6-2 f6-3) b))
                        (funcall #'%f6 b b b))))))
   7187592 -3970792748407 -14760)
  1 0)

;;; Wrong number of arguments passed to an anonymous function
(deftest misc.189
  (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (optimize (speed 3) (safety 1) (debug 1)))
       (let* ((v7 (labels ((%f13 (f13-1 f13-2 f13-3) 0))
		    (multiple-value-call #'%f13 (values a a a)))))
	 (flet ((%f10 nil v7)) (%f10)))))
   1733 3000 1314076)
  0)
