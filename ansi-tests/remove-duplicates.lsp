;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Sep 29 20:49:47 2002
;;;; Contains: Tests for REMOVE-DUPLICATES, DELETE-DUPLICATES

(in-package :cl-test)

(deftest random-remove-duplicates
  (loop for i from 1 to 5000
	always (random-test-remove-dups 20))
  t)

(deftest random-delete-duplicates
  (loop for i from 1 to 5000
	always (random-test-remove-dups 20 nil))
  t)

;;; Look for :KEY NIL bugs

(deftest remove-duplicates.1
  (let* ((orig '(1 2 3 4 1 3 4 1 2 5 6 2 7))
	 (x (copy-seq orig))
	 (y (remove-duplicates x :key nil)))
    (and (equalp orig x) y))
  (3 4 1 5 6 2 7))

(deftest delete-duplicates.1
  (let* ((orig '(1 2 3 4 1 3 4 1 2 5 6 2 7))
	 (x (copy-seq orig))
	 (y (delete-duplicates x :key nil)))
    y)
  (3 4 1 5 6 2 7))


;;; Order of evaluation tests

(deftest remove-duplicates.order.1
  (let ((i 0) a b c d e f)
    (values
     (remove-duplicates
      (progn (setf a (incf i)) (list 1 2 3 1 3 1 2 4))
      :from-end (progn (setf b (incf i)) nil)
      :start (progn (setf c (incf i)) 0)
      :end (progn (setf d (incf i)) nil)
      :key (progn (setf e (incf i)) #'identity)
      :test (progn (setf f (incf i)) #'=)
      )
     i a b c d e f))
  (3 1 2 4) 6 1 2 3 4 5 6)

(deftest remove-duplicates.order.2
  (let ((i 0) a b c d e f)
    (values
     (remove-duplicates
      (progn (setf a (incf i)) (list 1 2 3 1 3 1 2 4))
      :test-not (progn (setf b (incf i)) #'/=)
      :key (progn (setf c (incf i)) #'identity)
      :end (progn (setf d (incf i)) nil)
      :start (progn (setf e (incf i)) 0)
      :from-end (progn (setf f (incf i)) nil)
      )
     i a b c d e f))
  (3 1 2 4) 6 1 2 3 4 5 6)
 

;;; Keyword tests

(deftest remove-duplicates.allow-other-keys.1
  (remove-duplicates '(1 2 3 4 2 7 8 1 5) :allow-other-keys t)
  (3 4 2 7 8 1 5))

(deftest remove-duplicates.allow-other-keys.2
  (remove-duplicates '(1 2 3 4 2 7 8 1 5) :allow-other-keys nil)
  (3 4 2 7 8 1 5))

(deftest remove-duplicates.allow-other-keys.3
  (remove-duplicates '(1 2 3 4 2 7 8 1 5) :bad t :allow-other-keys t)
  (3 4 2 7 8 1 5))

(deftest remove-duplicates.allow-other-keys.4
  (remove-duplicates '(1 2 3 4 2 7 8 1 5) :allow-other-keys t :bad t)
  (3 4 2 7 8 1 5))

(deftest remove-duplicates.allow-other-keys.5
  (remove-duplicates '(1 2 3 4 2 7 8 1 5) :bad t
		     :allow-other-keys t :allow-other-keys nil)
  (3 4 2 7 8 1 5))

(deftest remove-duplicates.allow-other-keys.6
  (remove-duplicates '(1 2 3 4 2 7 8 1 5)
		     :allow-other-keys t :bad t :allow-other-keys nil)
  (3 4 2 7 8 1 5))

(deftest remove-duplicates.allow-other-keys.7
  (remove-duplicates '(1 2 3 4 2 7 8 1 5)
		     :allow-other-keys t :allow-other-keys nil :bad t)
  (3 4 2 7 8 1 5))

(deftest remove-duplicates.allow-other-keys.8
  (remove-duplicates '(1 2 3 4 2 7 8 1 5)
		     :allow-other-keys t :from-end t)
  (1 2 3 4 7 8 5))

(deftest remove-duplicates.keywords.1
  (remove-duplicates '(1 2 3 4 2 7 8 1 5) :from-end t :from-end nil)
  (1 2 3 4 7 8 5))


(deftest delete-duplicates.allow-other-keys.1
  (delete-duplicates (list 1 2 3 4 2 7 8 1 5) :allow-other-keys t)
  (3 4 2 7 8 1 5))

(deftest delete-duplicates.allow-other-keys.2
  (delete-duplicates (list 1 2 3 4 2 7 8 1 5) :allow-other-keys nil)
  (3 4 2 7 8 1 5))

(deftest delete-duplicates.allow-other-keys.3
  (delete-duplicates (list 1 2 3 4 2 7 8 1 5) :bad t :allow-other-keys t)
  (3 4 2 7 8 1 5))

(deftest delete-duplicates.allow-other-keys.4
  (delete-duplicates (list 1 2 3 4 2 7 8 1 5) :allow-other-keys t :bad t)
  (3 4 2 7 8 1 5))

(deftest delete-duplicates.allow-other-keys.5
  (delete-duplicates (list 1 2 3 4 2 7 8 1 5) :bad t
		     :allow-other-keys t :allow-other-keys nil)
  (3 4 2 7 8 1 5))

(deftest delete-duplicates.allow-other-keys.6
  (delete-duplicates (list 1 2 3 4 2 7 8 1 5)
		     :allow-other-keys t :bad t :allow-other-keys nil)
  (3 4 2 7 8 1 5))

(deftest delete-duplicates.allow-other-keys.7
  (delete-duplicates (list 1 2 3 4 2 7 8 1 5)
		     :allow-other-keys t :allow-other-keys nil :bad t)
  (3 4 2 7 8 1 5))

(deftest delete-duplicates.allow-other-keys.8
  (delete-duplicates (list 1 2 3 4 2 7 8 1 5)
		     :allow-other-keys t :from-end t)
  (1 2 3 4 7 8 5))

(deftest delete-duplicates.keywords.1
  (delete-duplicates (list 1 2 3 4 2 7 8 1 5) :from-end t :from-end nil)
  (1 2 3 4 7 8 5))

;;; Order of evaluation tests

(deftest delete-duplicates.order.1
  (let ((i 0) a b c d e f)
    (values
     (delete-duplicates
      (progn (setf a (incf i)) (list 1 2 3 1 3 1 2 4))
      :from-end (progn (setf b (incf i)) nil)
      :start (progn (setf c (incf i)) 0)
      :end (progn (setf d (incf i)) nil)
      :key (progn (setf e (incf i)) #'identity)
      :test (progn (setf f (incf i)) #'=)
      )
     i a b c d e f))
  (3 1 2 4) 6 1 2 3 4 5 6)

(deftest delete-duplicates.order.2
  (let ((i 0) a b c d e f)
    (values
     (delete-duplicates
      (progn (setf a (incf i)) (list 1 2 3 1 3 1 2 4))
      :test-not (progn (setf b (incf i)) #'/=)
      :key (progn (setf c (incf i)) #'identity)
      :end (progn (setf d (incf i)) nil)
      :start (progn (setf e (incf i)) 0)
      :from-end (progn (setf f (incf i)) nil)
      )
     i a b c d e f))
  (3 1 2 4) 6 1 2 3 4 5 6)
 
;;; Error cases

(deftest remove-duplicates.error.1
  (classify-error (remove-duplicates))
  program-error)

(deftest remove-duplicates.error.2
  (classify-error (remove-duplicates nil :start))
  program-error)

(deftest remove-duplicates.error.3
  (classify-error (remove-duplicates nil 'bad t))
  program-error)

(deftest remove-duplicates.error.4
  (classify-error (remove-duplicates nil 'bad t :allow-other-keys nil))
  program-error)

(deftest remove-duplicates.error.5
  (classify-error (remove-duplicates nil 1 2))
  program-error)

(deftest remove-duplicates.error.6
  (classify-error (remove-duplicates (list 'a 'b 'c) :test #'identity))
  program-error)

(deftest remove-duplicates.error.7
  (classify-error (remove-duplicates (list 'a 'b 'c) :test-not #'identity))
  program-error)

(deftest remove-duplicates.error.8
  (classify-error (remove-duplicates (list 'a 'b 'c) :key #'cons))
  program-error)

(deftest remove-duplicates.error.9
  (classify-error (remove-duplicates (list 'a 'b 'c) :key #'car))
  type-error)

;;;

(deftest delete-duplicates.error.1
  (classify-error (delete-duplicates))
  program-error)

(deftest delete-duplicates.error.2
  (classify-error (delete-duplicates nil :start))
  program-error)

(deftest delete-duplicates.error.3
  (classify-error (delete-duplicates nil 'bad t))
  program-error)

(deftest delete-duplicates.error.4
  (classify-error (delete-duplicates nil 'bad t :allow-other-keys nil))
  program-error)

(deftest delete-duplicates.error.5
  (classify-error (delete-duplicates nil 1 2))
  program-error)

(deftest delete-duplicates.error.6
  (classify-error (delete-duplicates (list 'a 'b 'c) :test #'identity))
  program-error)

(deftest delete-duplicates.error.7
  (classify-error (delete-duplicates (list 'a 'b 'c) :test-not #'identity))
  program-error)

(deftest delete-duplicates.error.8
  (classify-error (delete-duplicates (list 'a 'b 'c) :key #'cons))
  program-error)

(deftest delete-duplicates.error.9
  (classify-error (delete-duplicates (list 'a 'b 'c) :key #'car))
  type-error)