;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jul  6 06:11:01 2004
;;;; Contains: Tests of PPRINT-EXIT-IF-LIST-EXHAUSTED, PPRINT-POP

(in-package :cl-test)

(deftest pprint-exit-if-list-exhausted.1
  (with-standard-io-syntax
   (let ((*print-pretty* nil)
	 (*print-escape* nil)
	 (*print-right-margin* 100)
	 (*print-readably* nil)
	 )
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os '(1 2))
	(assert (equal (multiple-value-list
			(pprint-exit-if-list-exhausted))
		       '(nil)))
	(write (pprint-pop) :stream os)
	(assert (equal (multiple-value-list
			(pprint-exit-if-list-exhausted))
		       '(nil)))
	(write #\Space :stream os)
	(write (pprint-pop) :stream os)
	(pprint-exit-if-list-exhausted)
	(assert nil)))))
  "1 2")

(deftest pprint-exit-if-list-exhausted.2
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-escape* nil)
	 (*print-right-margin* 100)
	 (*print-readably* nil)
	 )
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os '(1 2))
	(assert (equal (multiple-value-list
			(pprint-exit-if-list-exhausted))
		       '(nil)))
	(write (pprint-pop) :stream os)
	(assert (equal (multiple-value-list
			(pprint-exit-if-list-exhausted))
		       '(nil)))
	(write #\Space :stream os)
	(write (pprint-pop) :stream os)
	(pprint-exit-if-list-exhausted)
	(assert nil)))))
  "1 2")

(deftest pprint-exit-if-list-exhausted.3
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-escape* nil)
	 (*print-right-margin* 100)
	 (*print-readably* nil)
	 )
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os '(1 . 2))
	(assert (equal (multiple-value-list
			(pprint-exit-if-list-exhausted))
		       '(nil)))
	(write (pprint-pop) :stream os)
	(write #\Space :stream os)
	(assert (equal (multiple-value-list
			(pprint-exit-if-list-exhausted))
		       '(nil)))
	(pprint-pop)
	(assert nil)))))
  "1 . 2")

(deftest pprint-exit-if-list-exhausted.4
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-escape* nil)
	 (*print-right-margin* 100)
	 (*print-readably* nil)
	 )
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os '(1 . 2) :prefix "[" :suffix "]")
	(assert (equal (multiple-value-list
			(pprint-exit-if-list-exhausted))
		       '(nil)))
	(write (pprint-pop) :stream os)
	(write #\Space :stream os)
	(assert (equal (multiple-value-list
			(pprint-exit-if-list-exhausted))
		       '(nil)))
	(pprint-pop)
	(assert nil)))))
  "[1 . 2]")

;;; Tests focusing on pprint-pop

(deftest pprint-pop.1
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-escape* nil)
	 (*print-right-margin* 100)
	 (*print-readably* nil)
	 (*print-length* 0))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os nil)
	(pprint-pop)
	(assert nil)))))
  "...")

(deftest pprint-pop.2
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-escape* nil)
	 (*print-right-margin* 100)
	 (*print-readably* nil)
	 (*print-length* 0))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os 1)
	(pprint-pop)))))
  "1")

(deftest pprint-pop.3
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-escape* nil)
	 (*print-right-margin* 100)
	 (*print-readably* nil)
	 (*print-length* 1))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os '(1))
	(assert (equal '(1) (multiple-value-list (pprint-pop))))))))
  "")

(deftest pprint-pop.4
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-escape* nil)
	 (*print-right-margin* 100)
	 (*print-readably* nil)
	 (*print-length* 0))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os '(1 2 3) :prefix "{" :suffix "}")
	(pprint-pop)
	(assert nil)))))
  "{...}")

(deftest pprint-pop.5
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-escape* nil)
	 (*print-right-margin* 100)
	 (*print-readably* nil)
	 (*print-length* 2))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os '(1 2 3 4 5) :prefix "{" :suffix "}")
	(pprint-exit-if-list-exhausted)
	(write (pprint-pop) :stream os)
	(loop (write #\Space :stream os)
	      (pprint-exit-if-list-exhausted)
	      (write (pprint-pop) :stream os))))))
  "{1 2 ...}")

(deftest pprint-pop.6
  (with-standard-io-syntax
   (let ((*print-pretty* t)
	 (*print-escape* nil)
	 (*print-right-margin* 100)
	 (*print-readably* nil)
	 (*print-length* 2))
     (with-output-to-string
       (os)
       (pprint-logical-block
	(os '(1 2 . 3) :prefix "{" :suffix "}")
	(pprint-exit-if-list-exhausted)
	(write (pprint-pop) :stream os)
	(loop (write #\Space :stream os)
	      (pprint-exit-if-list-exhausted)
	      (write (pprint-pop) :stream os))))))
  "{1 2 . 3}")

;;; Error cases

(deftest pprint-exit-if-list-exhausted.error.1
  (signals-error (pprint-exit-if-list-exhausted) error)
  t)

(deftest pprint-pop.error.1
  (signals-error (pprint-pop) error)
  t)




