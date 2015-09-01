;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan  6 05:41:06 2004
;;;; Contains: Tests of FILE-AUTHOR



(deftest file-author.1
  (loop for pn in
        (directory (make-pathname :name :wild :type :wild
                                  :defaults *default-pathname-defaults*))
        for author = (file-author pn)
        unless (or (null author) (stringp author))
        collect (list pn author))
  nil)

(deftest file-author.2
  (let ((author (file-author "sample-files/file-author.txt")))
    (if (or (null author) (stringp author))
        nil
      author))
  nil)

(deftest file-author.3
  (let ((author (file-author #p"sample-files/file-author.txt")))
    (if (or (null author) (stringp author))
        nil
      author))
  nil)

(deftest file-author.4
  (let ((author (file-author (truename "sample-files/file-author.txt"))))
    (if (or (null author) (stringp author))
        nil
      author))
  nil)

(deftest file-author.5
  (let ((author (with-open-file (s "sample-files/file-author.txt" :direction :input)
                                (file-author s))))
    (if (or (null author) (stringp author))
        nil
      author))
  nil)

(deftest file-author.6
  (let ((author (let ((s (open "sample-files/file-author.txt" :direction :input)))
                  (close s)
                  (file-author s))))
    (if (or (null author) (stringp author))
        nil
      author))
  nil)

;;; Specialized string tests

(deftest file-author.7
  (do-special-strings
   (s "sample-files/file-author.txt" nil)
   (assert (equal (file-author s) (file-author "sample-files/file-author.txt"))))
  nil)

;;; FIXME
;;; Add LPN test

;;; Error tests

(deftest file-author.error.1
  (signals-error (file-author) program-error)
  t)

(deftest file-author.error.2
  (signals-error (file-author "sample-files/file-author.txt" nil) program-error)
  t)

(deftest file-author.error.3
  (signals-error-always
   (file-author (make-pathname :name :wild :type "lsp"
                               :defaults *default-pathname-defaults*))
   file-error)
  t t)

(deftest file-author.error.4
  (signals-error-always
   (file-author (make-pathname :name "sample-files/file-author" :type :wild
                               :defaults *default-pathname-defaults*))
   file-error)
  t t)
