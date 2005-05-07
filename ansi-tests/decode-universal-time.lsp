;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May  7 07:00:58 2005
;;;; Contains: Tests of DECODE-UNIVERSAL-TIME

(in-package :cl-test)

(deftest decode-universal-time.1
  (decode-universal-time 0 0)
  0 0 0 1 1 1900 0 nil 0)

(deftest decode-universal-time.2
  (decode-universal-time 0 -1)
  0 0 1 1 1 1900 0 nil -1)

(deftest decode-universal-time.3
  (let ((count 0))
    (loop for time = (random 10000000000)
	  for tz = (- (random 49) 24)
	  for (second minute hour date month year day daylight-p zone)
	  = (multiple-value-list (decode-universal-time time tz))
	  for time2 = (encode-universal-time second minute hour date month year zone)
	  repeat 1000
	  unless (and (eql tz zone) (eql time time2) (null daylight-p))
	  collect (progn (incf count)
			 (list time tz (list second minute hour date month year day daylight-p zone) time2))
	  until (>= count 100)))
  nil)

(deftest decode-universal-time.4
  (let ((count 0))
    (loop for time = (random 10000000000)
	  for tz = (/ (- (random (1+ (* 48 3600))) (* 24 3600)) 3600)
	  for (second minute hour date month year day daylight-p zone)
	  = (multiple-value-list (decode-universal-time time tz))
	  for time2 = (encode-universal-time second minute hour date month year zone)
	  repeat 1000
	  unless (and (eql tz zone) (eql time time2) (null daylight-p))
	  collect (progn (incf count)
			 (list time tz (list second minute hour date month year day daylight-p zone) time2))
	  until (>= count 100)))
  nil)

(deftest decode-universal-time.5
  (let ((count 0))
    (loop for time = (let ((x (random 10000000000))) (print x) x)
	  for (second minute hour date month year day daylight-p zone)
	  = (multiple-value-list (decode-universal-time time))
	  for time2 = (encode-universal-time second minute hour date month year)
	  repeat 1000
	  unless (eql time time2)
	  collect (progn (incf count)
			 (list time (list second minute hour date month year day daylight-p zone) time2))
	  until (>= count 100)))
  nil)

;;; Error tests

(deftest decode-universal-time.error.1
  (signals-error (decode-universal-time) program-error)
  t)

(deftest decode-universal-time.error.2
  (signals-error (decode-universal-time 0 0 nil) program-error)
  t)




