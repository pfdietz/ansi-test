;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Feb 28 21:59:57 2003
;;;; Contains: Tests of INVOKE-DEBUGGER

(in-package :cl-test)

;;; We can't test actual entry into the debugger, but we can test
;;; that the function in *debugger-hook* is properly called.

(deftest invoke-debugger.1
  (block done
   (let (fn (cnd (make-condition 'simple-error)))
       (setq fn #'(lambda (c hook)
		    (return-from done
		      (and (null *debugger-hook*)
			   (eqt hook fn)
			   (eqt cnd c)
			   'good))))
       (let ((*debugger-hook* fn))
	 (invoke-debugger cnd)))
   'bad)
  good)
