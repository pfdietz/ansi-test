;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan  6 05:32:37 2004
;;;; Contains: Tests of TRUENAME



(deftest truename.1
  (let* ((pn #p"sample-files/truename.txt")
         (tn (truename pn)))
    (values
     (notnot (pathnamep pn))
     (typep pn 'logical-pathname)
     (equalt (pathname-name pn) (pathname-name tn))
     (equalt (pathname-type pn) (pathname-type tn))
     ))
  t nil t t)

(deftest truename.2
  (let* ((name "sample-files/truename.txt")
         (pn (pathname name))
         (tn (truename name)))
    (values
     (notnot (pathnamep pn))
     (typep pn 'logical-pathname)
     (equalt (pathname-name pn) (pathname-name tn))
     (equalt (pathname-type pn) (pathname-type tn))
     ))
  t nil t t)

(deftest truename.3
  (let* ((pn #p"sample-files/truename.txt"))
    (with-open-file
     (s pn :direction :input)
     (let ((tn (truename s)))
       (values
        (notnot (pathnamep pn))
        (typep pn 'logical-pathname)
        (equalt (pathname-name pn) (pathname-name tn))
        (equalt (pathname-type pn) (pathname-type tn))
        ))))
  t nil t t)

(deftest truename.4
  (let* ((pn #p"sample-files/truename.txt"))
    (let ((s (open pn :direction :input)))
      (close s)
      (let ((tn (truename s)))
        (values
         (notnot (pathnamep pn))
         (typep pn 'logical-pathname)
         (equalt (pathname-name pn) (pathname-name tn))
         (equalt (pathname-type pn) (pathname-type tn))
         ))))
  t nil t t)

(deftest truename.5
  (let* ((lpn "CLTEST:scratch/foo.txt")
         (pn (translate-logical-pathname lpn)))
    (unless (probe-file lpn)
      (with-open-file (s lpn :direction :output) (format s "Stuff~%")))
    (let ((tn (truename lpn)))
      (values
       (notnot (pathnamep pn))
       (if (equalt (pathname-name pn) (pathname-name tn))
           t (list (pathname-name pn) (pathname-name tn)))
       (if (equalt (pathname-type pn) (pathname-type tn))
           t (list (pathname-type pn) (pathname-type tn)))
       )))
  t t t)

;;; Specialized string tests

(deftest truename.6
  (do-special-strings
   (s "sample-files/truename.txt" nil)
   (assert (equalp (truename s) (truename "sample-files/truename.txt"))))
  nil)

;;; Error tests

(deftest truename.error.1
  (signals-error (truename) program-error)
  t)

(deftest truename.error.2
  (signals-error (truename "sample-files/truename.txt" nil) program-error)
  t)

(deftest truename.error.3
  (signals-error-always (truename "sample-files/nonexistent") file-error)
  t t)

(deftest truename.error.4
  (signals-error-always (truename #p"sample-files/nonexistent") file-error)
  t t)

(deftest truename.error.5
    (signals-error-always
     (truename
      (logical-pathname "CLTESTROOT:sample-files/nonexistent"))
  file-error) t t)

(deftest truename.error.6
  (signals-error-always
   (let ((pn (make-pathname :name :wild
                            :defaults *default-pathname-defaults*)))
     (truename pn))
   file-error)
  t t)
