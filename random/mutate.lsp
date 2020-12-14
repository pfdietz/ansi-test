;-*- Mode:     Lisp -*-

(in-package :cl-test)

(defparameter *lambdas* #())
(defparameter *dump-stream* nil)

;;; Include some standard packages in CL-TEST to make randomly
;;; read code more understandable

(defparameter *standard-packages-to-include*
  '(:alexandria :iterate :serapeum :named-readtables
    :curry-compose-reader-macros :closer-mop
    :cl-ppcre :split-sequence :trivial-features
    :trivial-gray-streams :bordeaux-threads
    :anaphora :let-plus :fiveam :lift :trivial-garbage
    :flexi-streams :nibbles :ironclad
    :puri :usocket :trivial-backtrace
    :more-conditions :chunga :cl-fad
    :cl+ssl :cl-base64 :esrap :chipz
    :drakma :local-time
    :parse-number :cl-json
    ))

(defun include-std-packages (&aux (cltp (find-package "CL-TEST")))
  (assert cltp)
  (when (find-package :ql)
    (ql:quickload :named-readtables)
    (ql:quickload :curry-compose-reader-macros)
    (loop for e in *standard-packages-to-include*
          do (let ((system (if (symbolp e) e (car e)))
                   #+nil (package-name (if (symbolp e) e (cadr e)))
                   )
               (funcall (intern "QUICKLOAD" :QL) system)
               (unless (eql (find-package "CL-TEST") cltp)
                 (error "Reading ~a redefined CL-TEST" system))
               #+nil
               (let ((package (find-package package-name)))
                 (if package
                     (handler-bind
                         ((package-error
                            (lambda (e)
                              (let ((r (or (find-restart 'sb-impl::shadowing-import-it e)
                                           (find-restart 'sb-impl::take-new))))
                                (when r
                                  (invoke-restart r))))))
                       (use-package (list package) :cl-test))
                     (warn "Could not find package ~a"
                           (string package-name))))))))

;;; Mutation testing of Lisp functions to find compiler bugs
;;; Assumes we can recognize 'abnormal' errors thrown by the compiler

(defun default-mutate-sexpr (sexpr &optional (point-mutate-fn #'default-point-mutate-fn))
  ;; Finds a random point in an s-expression, then mutates it using
  ;; point-mutate-fn
  (let ((new-sexpr nil))
    (loop do (setf new-sexpr (let ((random-point (find-random-point-in-sexpr sexpr)))
                               (mutate-point-in-sexpr sexpr random-point point-mutate-fn)))
       while (equal sexpr new-sexpr))
    new-sexpr))

(defun default-point-mutate-fn (e)
  (declare (ignore e))
  (rcase
    (1 (random-from-seq '(a b c d e f g)))
    (1 (random 1000))
    ))

(defun random-subexpr-of-random-lambda (e)
  (declare (ignore e))
  "Given a lambda expression E, returns a random subexpression.  May be sloppy and give a nonsense form."
  (let* ((len (length *lambdas*))
         (i (random len))
         (lam (elt *lambdas* i))
         (sexpr (cddr lam)))
    ;; strip off docstrings and declares
    (loop while (and (consp sexpr)
                     (or (and (consp (car sexpr))
                              (eql (caar sexpr) 'declare))
                         (and (stringp (car sexpr))
                              (cdr sexpr))))
       do (pop sexpr))
    ;; If it's a singleton body, just use that form
    (when (null (cdr sexpr))
      (setf sexpr (car sexpr)))
    (subexpr-at-point sexpr (find-random-point-in-sexpr sexpr))))

;; A size tree decorates the nodes of an s-expression with size
;; information, so random sub-expressions can be efficiently found.
(defstruct size-tree-node
  (size 0 :type (integer 0))
  (children nil :type list))

;; "Points" are addresses of subexpressions.
(defun mutate-point-in-sexpr (sexpr point point-mutate-fn)
  "Functional replacement of a subexpression of sexpr, addressed by point,
   with the value returned by mutating it with point-mutate-fn"
  (if (null point)
      (funcall point-mutate-fn sexpr)
      (ecase (pop point)
        (0 (let ((new-car (mutate-point-in-sexpr (car sexpr) point point-mutate-fn)))
             (cons new-car (cdr sexpr))))
        (1 (let ((new-cdr (mutate-point-in-sexpr (cdr sexpr) point point-mutate-fn)))
             (cons (car sexpr) new-cdr))))))           

(defun find-random-point-in-sexpr (sexpr)
  "Returns a random subexpression of sexpr"
  ;; Build a tree of sizes
  (let ((size-tree (size-tree-of-sexpr sexpr)))
    ;; size-tree is a tree of size-tree-node records
    (let ((n (random (size-tree-node-size size-tree))))
      (reverse
       (find-point-in-sexpr-with-sizes sexpr size-tree n nil)))))

(defun subexpr-at-point (sexpr point)
  (if (null point)
      sexpr
      (ecase (pop point)
        (0 (subexpr-at-point (car sexpr) point))
        (1 (subexpr-at-point (cdr sexpr) point)))))

(defun random-subexpr-of-sexpr (sexpr)
  (let ((point (find-random-point-in-sexpr sexpr)))
    (subexpr-at-point sexpr point)))

(defun find-point-in-sexpr-with-sizes (sexpr size-tree n path)
  (loop for c in (size-tree-node-children size-tree)
       for i from 0
     do (let ((c-n (size-tree-node-size c)))
          (if (>= n c-n)
              (decf n c-n)
              (return-from find-point-in-sexpr-with-sizes
                (find-point-in-sexpr-with-sizes
                 (ecase i
                   (0 (car sexpr))
                   (1 (cdr sexpr)))
                 c
                 n
                 (cons i path))))))
  path)

(defparameter *size-tree-of-sexpr-cache* (make-hash-table))

(defun size-tree-of-sexpr (sexpr)
  (let ((cache *size-tree-of-sexpr-cache*))
    (or (gethash sexpr cache)
        (setf (gethash sexpr cache)
              (size-tree-of-sexpr* sexpr)))))

(defun size-tree-of-sexpr-cache-clear ()
  (clrhash *size-tree-of-sexpr-cache*))

(defun size-tree-of-sexpr* (sexpr)
  (if (consp sexpr)
      (let ((reversed nil))
        (loop while (consp sexpr)
              do (push (pop sexpr) reversed))
        (let ((node (size-tree-of-sexpr* sexpr)))
          (loop while reversed
                do (let ((car-node (size-tree-of-sexpr* (pop reversed))))
                     (setf node (make-size-tree-node
                                 :size (+ (size-tree-node-size car-node)
                                          (size-tree-node-size node))
                                 :children (list car-node node)))))
          node))
      (make-size-tree-node :size 1 :children nil)))

;;; Attempt to get a SB-INT:BUG condition from mutated lambda exprs

(defvar *current-lambda* nil)

(defun proper-listp (x)
  ;; doesn't work on circular lists
  (loop while (consp x) do (pop x))
  (null x))

(defun trim-improper-lists (root)
  "Non-destructively trim off the non-nil tail element of the improper lists under ROOT."
  (labels ((%t (x)
             (if (consp x)
                 (let* ((y x)
                        (result (loop while (consp y)
                                   collect (%t (pop y)))))
                   (if (or y (notevery #'eq x result))
                       result
                       x))
                 x)))
    (%t root)))

(defun mutate-sexpr-fn-for-fn (fn)
  (lambda (sexpr ignored-fn)
    (declare (ignore ignored-fn))
    (default-mutate-sexpr sexpr
        #'(lambda (e)
            (funcall fn e)))))

(defun mutate-sexpr-the-nil (sexpr ignored-fn)
  (declare (ignore ignored-fn))
  (default-mutate-sexpr sexpr
      #'(lambda (e)
          `(flet ((the-nil-f () (the nil ,e))) (the-nil-f)))))

(defun mutate-sexpr-throw (sexpr ignored-fn)
  (declare (ignore ignored-fn))
  (default-mutate-sexpr sexpr
      #'(lambda (e) `(throw 'done ,e))))

(defun mutate-sexpr-reot (sexpr ignored-fn)
  (declare (ignore ignored-fn))
  (default-mutate-sexpr sexpr #'(lambda (e)
                                  (let* ((y (make-random-element-of-type t))
                                         (x (if (and y (or (and (not (keywordp y))
                                                                (symbolp y)))
                                                        (consp y))
                                                `(quote ,y)
                                                y)))
                                    (if (listp e) (list x) x)))))

(defun mutate-sexpr-error (sexpr ignored-fn)
  (declare (ignore ignored-fn))
  (default-mutate-sexpr sexpr #'(lambda (e)
                                  (declare (ignore e))
                                  (let ((x `(error ,(format nil "~a" (random 1000)))))
                                    x))))

(defvar *cl-syms* nil)

(defun compute-cl-syms ()
  (or *cl-syms*
      (setf *cl-syms*
            (let ((result nil))
              (do-external-symbols (s "COMMON-LISP")
                (unless (has-prohibited-symbol (list nil s))
                  (push s result)))
              (coerce result 'vector)))))

(defun mutate-sexpr-cl-sym (sexpr ignored-fn)
  (declare (ignore ignored-fn))
  (let* ((s (compute-cl-syms))
         (l (length s)))
    (default-mutate-sexpr sexpr
        #'(lambda (e) (unless (listp e) (svref s (random l)))))))

(defun mutate-sexpr-go (sexpr ignored-fn)
  (declare (ignore ignored-fn))
  (default-mutate-sexpr sexpr
      #'(lambda (e) (declare (ignore e)) '(go 17))))

(declaim (ftype (function (t t) t) mutate-sexpr-bad-floats))

(let ((bad-floats
       (let ((vals
              (list sb-ext:double-float-negative-infinity
                    sb-ext:double-float-positive-infinity
                    sb-ext:single-float-negative-infinity
                    sb-ext:single-float-positive-infinity
                    sb-ext:long-float-negative-infinity
                    sb-ext:long-float-positive-infinity
                    sb-ext:short-float-negative-infinity
                    sb-ext:short-float-positive-infinity)))
         (coerce (remove-duplicates vals) 'vector))))
  (defun mutate-sexpr-bad-floats (sexpr ignored-fn)
    (declare (ignore ignored-fn))
    (default-mutate-sexpr sexpr
        #'(lambda (e)
            (unless (listp e) (random-from-seq bad-floats))))))

(defun mutate-sexpr-satisfies-eval (sexpr ignored-fn)
  (declare (ignore ignored-fn))
  (default-mutate-sexpr sexpr
      #'(lambda (e)
          `(the (satisfies eval) ,e))))

(defun mutate-sexpr-the-simple-array-nil (sexpr ignored-fn)
  (declare (ignore ignored-fn))
  (default-mutate-sexpr sexpr
      #'(lambda (e)
          `(the (simple-array nil) ,e))))

(defun mutate-sexpr-class-of (sexpr ignored-fn)
  (declare (ignore ignored-fn))
  (default-mutate-sexpr sexpr
      #'(lambda (e)
          `(class-of ,e))))

(defun safe-length (e)
  (let ((i 0))
    (declare (fixnum i))
    (loop while (consp e)
       do (incf i)
       do (pop e))
    i))

(defun mutate-sexpr-misc (sexpr ignored-fn)
  (declare (ignore ignored-fn))
  (default-mutate-sexpr sexpr
      #'(lambda (e)
          (if (consp e)
              (if (and (eql (car e) 'quote)
                       (consp (cadr e))
                       (proper-listp (cadr e)))
                  `(quote ,(append (butlast (cadr e))
                                   (car (last (cadr e)))))
                  (let ((len (safe-length e)))
                    (rcase
                      (1 (if (proper-listp e)
                             (random-permute e)
                             (append (random-permute (subseq e 0 len))
                                     (cdr (last e)))))
                      (1 (cdr e))
                      ;; (1 (if (not (stringp (car e))) (car e) (cdr e)))
                      (1 (list (nthcdr (1+ (random len)) e)))
                      (1 (list (elt e (random len))))
                      (1 (if (consp (cdr e))
                             (list* (cadr e) (car e) (cddr e))
                             (cdr e))))))
              (list e)))))

#|
(let ((nan (sb-int:with-float-traps-masked (:invalid) (/ 0d0 0d0))))
  (defun mutate-sexpr-nan (sexpr ignored-fn)
    (declare (ignore ignored-fn))
    (default-mutate-sexpr sexpr
        #'(lambda (e)
            (unless (listp e) nan)))))
|#

(defun mutate-sexpr-random-lambda (sexpr ignored-fn)
  (declare (ignore ignored-fn))
  (default-mutate-sexpr sexpr (lambda (e)
                                (let ((se (random-subexpr-of-random-lambda e)))
                                  (when (or (not (listp e))
                                            (proper-listp se))
                                    se)))))

(defun mutate-sexpr-n-random-lambda (sexpr ignored-fn n)
  (loop repeat n
     do (setf sexpr (mutate-sexpr-random-lambda sexpr ignored-fn)))
  sexpr)

(defun mutation-test-lambda-expr (lambda-expr &key (n 100) (do? nil)
                                                (reps 1)
                                                (mutate-sexpr #'default-mutate-sexpr)
                                                &allow-other-keys)
  (assert (eql (car lambda-expr) 'lambda))
  (assert (listp (cadr lambda-expr)))
  (let ((lambda-prefix (subseq lambda-expr 0 2))
        (lambda-body (cddr lambda-expr))
        (*error-output* (make-broadcast-stream)))
    (loop repeat n
       do (let ((mutated-body lambda-body))
            (loop repeat reps
               do  (let* ((subexpr (if do? '(do)
                                       (random-subexpr-of-sexpr lambda-body))))
                     (setf mutated-body (funcall
                                         mutate-sexpr
                                         lambda-body
                                         #'(lambda (e)
                                             (when (or (not (listp e))
                                                       (proper-listp subexpr))
                                               subexpr))))))
            (let ((mutated-lambda (append lambda-prefix mutated-body)))
              (setf *current-lambda* mutated-lambda)
              (when *dump-stream*
                (format *dump-stream* "~A~%" mutated-lambda)
                (finish-output *dump-stream*))
              (unless (is-bad-mutated-form mutated-lambda)
                (handler-case
                    #+sbcl
                  (sb-ext:with-timeout 10
                    (compile nil mutated-lambda))
                  #-sbcl
                  (compile nil mutated-lambda)
                  #+sbcl (sb-int:bug () (return mutated-lambda))
                  #+sbcl (sb-ext:timeout () nil)
                  ;; (storage-condition () nil)
                  ;; (error () nil)
                  #+sbcl
                  (sb-int:simple-program-error (e)
                    (unless (equal (car (simple-condition-format-arguments e))
                                   "Required argument")
                      (return mutated-lambda)))
                  #+sbcl
                  (sb-kernel::arg-count-error (e)
                    (unless (or (eql (sb-kernel::defmacro-lambda-list-bind-error-kind e)
                                     'destructuring-bind)
                                (eql (sb-kernel::defmacro-lambda-list-bind-error-name e) 'lambda)
                                )
                      (return mutated-lambda)))
                  #+sbcl
                  (sb-sys:interactive-interrupt (c) (error c))
                  #+sbcl
                  (sb-sys:system-condition ()
                    (return mutated-lambda))
                  #+sbcl
                  (simple-type-error (e)
                    (unless (eql (type-error-expected-type e) 'sb-impl::function-name)
                      (return mutated-lambda)))
                  #|
                  (simple-error (e)
                  (unless (equal (simple-condition-format-arguments e)
                  '((NOT #1=(EQL (SB-C::LAMBDA-KIND SB-C::CLAMBDA) :DELETED)) ((#1# T))))
                  (return mutated-lambda)))
                  |#
                  
                  #|
                  (type-error (e)
                  (unless (eql (sb-kernel::type-error-context e) 'sb-impl::x)
                  (return mutated-lambda)))
                  |#
                  #+clisp
                  (system::simple-source-program-error () nil)
                  #+clisp
                  (system::simple-program-error () nil)
                  #-clisp
                  (error () (return mutated-lambda))
                  #+clisp
                  (error () nil)
                  )))))))

(defparameter *results* nil)
(defparameter *dump-stream* nil)

(defmacro with-dump (&body body)
  `(with-open-file (*dump-stream* "dump.out" :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
     ,@body))

(defun loop-mt (&rest args &key &allow-other-keys)
  (size-tree-of-sexpr-cache-clear)
  (loop
       (let ((result (apply #'loop-mutation-test args)))
         (print result)
         (terpri)
         (push result *results*))))

(defun loop-mt2 (&optional (reps 1))
  (loop-mt :lambda-expr-fn #'make-random-from-lambdas
     :mutate-sexpr #'mutate-sexpr-random-lambda
     :reps reps))

#|
(defun loop-mt2-n (&optional (n 2))
  (loop-mt :lambda-expr-fn #'make-random-from-lambdas
     :mutate-sexpr #'(lambda (sexpr ignored-fn)
                       (mutate-sexpr-n-random-lambda sexpr ignored-fn n))))
|#

(defun loop-mt3 (&key (size 25) (n 20) (reps 1))
  (loop-mt :size size :n n :reps reps
     :mutate-sexpr #'mutate-sexpr-random-lambda))

(defun loop-mt4 (&optional (reps 1))
  (loop-mt :n 20 :reps reps
     :lambda-expr-fn #'make-random-from-lambdas
     :mutate-sexpr #'mutate-sexpr-reot))

(defun loop-mt5 ()
  (loop-mt :n 20
     :lambda-expr-fn #'make-random-from-lambdas
     :mutate-sexpr #'mutate-sexpr-go))

(defun loop-mt6 ()
  (loop-mt :n 20
     :lambda-expr-fn #'make-random-from-lambdas
     :mutate-sexpr #'mutate-sexpr-throw))

(defun loop-mt7 ()
  (loop-mt :n 20
     :lambda-expr-fn #'make-random-from-lambdas
     :mutate-sexpr #'mutate-sexpr-cl-sym))

(defun loop-mt8 ()
  (loop-mt :n 20
     :lambda-expr-fn #'make-random-from-lambdas
     :mutate-sexpr #'mutate-sexpr-bad-floats))

#|
(defun loop-mt9 ()
  (loop-mt :n 20
     :lambda-expr-fn #'make-random-from-lambdas
     :mutate-sexpr #'mutate-sexpr-nan))
|#

(defun loop-mt10 ()
  (loop-mt :n 20
     :lambda-expr-fn #'make-random-from-lambdas
     :mutate-sexpr #'mutate-sexpr-satisfies-eval))

(defun loop-mt10a (&key size)
  (loop-mt :n 20 :size size
     :mutate-sexpr #'mutate-sexpr-satisfies-eval))

(defun loop-mt11 ()
  (loop-mt :n 20
     :lambda-expr-fn #'make-random-from-lambdas
     :mutate-sexpr #'mutate-sexpr-the-simple-array-nil))

(defun loop-mt12 ()
  (loop-mt :n 20
     :lambda-expr-fn #'make-random-from-lambdas
     :mutate-sexpr #'mutate-sexpr-misc))

(defun loop-mt13 (&key (n 20) (reps 1))
  (loop-mt :n n :reps reps
     :lambda-expr-fn #'make-random-from-lambdas
     :mutate-sexpr #'mutate-sexpr-class-of))

(defun loop-mt14 (&key (n 20) (reps 1))
  (loop-mt :n n :reps reps
     :lambda-expr-fn #'make-random-from-lambdas
           :mutate-sexpr #'mutate-sexpr-error))

#|
(defun loop-mt15 (&key (reps 1))
  (loop-mt :lambda-expr-fn #'make-random-from-lambdas
           :mutate-sexper #'mutate-sexpr-random-permute
           :reps reps))
|#

(defun loop-mutation-test (&rest args &key (n 100) (size 50)
                                        (reps 1)
                                        (lambda-expr-fn #'make-random-int-lambda-expr)
                                        cache?
                                        &allow-other-keys)
  (let ((*vars* nil)
        result)
    (loop for i from 1
       do (setf result (let ((lambda-expr (funcall lambda-expr-fn size)))
                         (apply #'mutation-test-lambda-expr lambda-expr :n n :reps reps args)))
       do (when result (return result))
       do (when (eql (mod i 10) 0)
            (unless cache? (size-tree-of-sexpr-cache-clear))
            (format t "~A " i)
            (when (eql (mod i 200) 0) (terpri))
            (finish-output)))))

(defun make-random-int-lambda-expr (size)
  (let ((form (make-random-integer-form size)))
    `(lambda () ,form)))

;;; Utility functions to read the defuns of a file

(defun read-exprs-from-file (fn)
  (handler-case
      (with-open-file (s fn :direction :input)
        (read-exprs-from-stream s))
    (file-error () nil)))

(defun read-exprs-from-stream (s &key (named-readtables t))
  "Read exprs from stream until all are done or there is an irrecoverable error.  Return the list of exprs."
  (let ((forms nil)
        (*read-eval* nil)
        (*readtable* *readtable*)
        (*package* *package*))
    (handler-case
        (loop for x = (handler-case (read s nil s) (reader-error () nil))
           until (eql x s)
           do (cond
                ((and named-readtables (consp x) (symbolp (car x))
                      (equal (symbol-name (car x)) "IN-READTABLE"))
                 (handler-case (eval (cons (find-symbol "IN-READTABLE"
                                                        :named-readtables)
                                           (cdr x)))
                   (error () nil)))
                ((and (consp x) (eql (car x) 'in-package))
                 (let ((n (cadr x)))
                   (let ((p (find-package n)))
                     (when p (setf *package* p)))))
                ((and (consp x) (eql (car x) 'defpackage)
                      (not (find-package (cadr x))))
                 ;; :LOCK was causing spurious failures in tests
                 ;; derived from ESRAP
                 (setf x
                       (remove-if (lambda (c)
                                    (and (consp c) (eql (car c) :lock)))
                                  x))
                 (handler-case (eval x) (error () nil)))
                (t
                 (push x forms))))
      (error () nil))
    (nreverse forms)))

(defun read-defuns-from-stream (s &key (sym 'defun))
  ;; Reads the top level DEFUNs from a stream.
  ;; :SYM keyword allows other forms to be found instead
  ;; Do not respect in-package forms, and if undefined
  ;; packages are found use *package* instead (in SBCL)
  (let ((*package* *package*)
        (*read-eval* nil))
    (handler-case
        (loop for x = (handler-case
                          ;; Recent SBCLs have been provided with a restart
                          ;; when a package is not found.  Use *package* instead
                          ;; for these cases.
                          (handler-bind (#+sbcl
                                         (sb-int:simple-reader-package-error
                                          (lambda (e) (use-value *package* e))))
                            (read s nil s))
                        (reader-error () nil))
           until (eql x s)
           when (and (consp x) (eql (car x) sym))
           collect (progn
                     ;; (format t "~A~%" (cadr x))
                     x))
      (error () nil))))

(defun read-defuns (filename &rest args)
  "Read DEFUNS from a file, returning them.  :SYM argument allows
forms starting with other symbols to be found instead."
  (format t "Reading from ~A~%" filename)
  (finish-output *standard-output*)
  (with-open-file (s filename :direction :input)
    (apply #'read-defuns-from-stream s args)))

(defun size-of-sexpr (s)
  (let ((tree (size-tree-of-sexpr s)))
    (size-tree-node-size tree)))

(defun lambda-of-defun (d)
  (assert (and (consp d) (eql (car d) 'defun)))
  (cons 'lambda (cddr d)))

(defun lambda-of-defmethod (d)
  (assert (and (consp d) (eql (car d) 'defmethod)))
  (let ((args (mapcar
	       (lambda (a) (etypecase a
			     (symbol a)
			     (cons (car a))
			     (t (return-from lambda-of-defmethod nil))))
	       (third d)))
	(body (cdddr d)))
    `(lambda ,args ,@body)))

(defun files-of-dirs (dirs)
  (reduce #'append (cons nil (mapcar #'directory dirs)) :from-end t))

(defun filter-lambdas (lambda-list)
  (remove-small-lambdas
   (mapcar #'remove-declares-from-tree
           (remove-if #'has-prohibited-symbol
                      (mapcar #'trim-improper-lists
                              (remove-if #'is-circular-tree? lambda-list))))))

(defun remove-small-lambdas (ll)
  (remove-if (lambda (x)
               (or (not (listp (cadr x)))
                   (and (null (cdddr x))
                        (not (consp (caddr x))))))
             ll))

(defun lambdas-of-defuns (&optional (dirs '("**/*.lsp")))
  (let* ((files (files-of-dirs dirs))
         (defuns (loop for fn in files
                    append (read-defuns fn))))
    (filter-lambdas (mapcar #'lambda-of-defun defuns))))

(defun lambda-of-deftest (dt)
  `(lambda () ,(third dt)))

(defun lambdas-of-deftests (&optional (dirs '("**/*.lsp")))
  (let* ((files (files-of-dirs dirs))
         (tests (loop for fn in files
                   append (read-defuns fn :sym 'deftest))))
    (filter-lambdas (mapcar #'lambda-of-deftest tests))))

(defun lambdas-of-defs (&optional (dirs '("**/*.lsp")))
  (let* ((files (files-of-dirs dirs))
         (exprs (mapcan #'read-exprs-from-file files)))
    (filter-lambdas
     (mapcan #'lambdas-of-def-expr exprs))))

(defgeneric lambdas-of-def-expr (expr)
  (:documentation "Returns a list of lambda expressions mined from a definition expr EXPR, or NIL if none could be found."))

(defmethod lambdas-of-def-expr ((expr cons))
  (lambdas-of-def-expr-cons (car expr) expr))

(defparameter *all-exprs-to-lambdas* nil
  "When true, convert every form to a lambda")

(defmethod lambdas-of-def-expr (expr) nil)
#|
  (if (and *all-exprs-to-lambdas*
           (consp expr)
           (not (eql (car expr) 'quote)))
      `(lambda () ,expr)
      nil)
|#

(defgeneric lambdas-of-def-expr-cons (car-of-expr expr))

(defmethod lambdas-of-def-expr-cons ((car-of-expr (eql 'defun)) expr)
  (list (lambda-of-defun expr)))

(defmethod lambdas-of-def-expr-cons ((car-of-expr (eql 'deftest)) expr)
  (list (lambda-of-deftest expr)))

(defmethod lambdas-of-def-expr-cons ((car-of-expr (eql 'defmethod)) expr)
  (lambdas-of-defmethod expr))

(defmethod lambdas-of-def-expr-cons ((car-of-expr (eql 'defgeneric)) expr)
  (lambdas-of-defgeneric expr))

(defmethod lambdas-of-def-expr-cons ((car-of-expr (eql 'def)) expr)
  (when (consp (cdr expr))
    (cond
      ((or (member (cadr expr) '(function test partial-eval-test))
           (and (consp (cadr expr))
                (eql (caadr expr) 'function)))
       (lambdas-of-def-expr `(defun ,(caddr expr) ,@(cdddr expr))))
      ((eql (cadr expr) 'method)
       (lambdas-of-defmethod `(defmethod ,@(cddr expr)))))))

(defmethod lambdas-of-def-expr-cons ((car-of-expr (eql 'quote)) (expr t)) nil)
(defmethod lambdas-of-def-expr-cons ((car-of-expr (eql 'defclass)) (expr t)) nil)

;; (defmethod lambdas-of-def-expr-cons ((car-of-expr (eql 'defmacro)) expr)
;;  (lambdas-of-defmacro expr))

(defun safe-mapcan (fn arg)
  (loop while (consp arg)
        append (funcall fn (pop arg))))

(defmethod lambdas-of-def-expr-cons ((car-of-expr (eql 'eval-when)) expr)
  (when (consp (cdr expr))
    (let ((body (cddr expr)))
      (safe-mapcan #'lambdas-of-def-expr body))))

(defmethod lambdas-of-def-expr-cons ((car-of-expr (eql 'let)) expr)
  (when (consp (cdr expr))
    (let ((body (cddr expr)))
      (safe-mapcan #'lambdas-of-def-expr body))))

(defmethod lambdas-of-def-expr-cons ((car-of-expr (eql 'let*)) expr)
  (when (consp (cdr expr))
    (let ((body (cddr expr)))
      (safe-mapcan #'lambdas-of-def-expr body))))

(defmethod lambdas-of-def-expr-cons ((car-of-expr (eql 'progn)) expr)
  (let ((body (cdr expr)))
    (safe-mapcan #'lambdas-of-def-expr body)))

(defmethod lambdas-of-def-expr-cons ((car-of-expr (eql 'locally)) expr)
  (let ((body (cdr expr)))
    (safe-mapcan #'lambdas-of-def-expr body)))

(defmethod lambdas-of-def-expr-cons ((x (eql 'in-package)) y) nil)
(defmethod lambdas-of-def-expr-cons ((x (eql 'defconstant)) y) nil)
(defmethod lambdas-of-def-expr-cons ((x (eql 'defparameter)) y) nil)
(defmethod lambdas-of-def-expr-cons ((x (eql 'defvar)) y) nil)

(defparameter *failed-defs* nil)

(defmethod lambdas-of-def-expr-cons ((x symbol) y)
  (cond
    ((equal (symbol-name x) "DEF")
     (lambdas-of-def-expr-cons 'def (cons 'def (cdr y))))
    (t
     `((lambda () ,y)))))

(defmethod lambdas-of-def-expr-cons (x y)
  nil) ;; default method

(defun lambdas-of-defmethod (defmethod-expr)
  (lambdas-of-defmethod-body (cdr defmethod-expr)))

(defun lambdas-of-defmethod-body (rest)
  (and (consp rest)
       (progn
         ;; Get rid of name and method qualifiers
         (loop do (pop rest) while (and (consp rest) (not (listp (car rest)))))
         (and (consp rest)
              (let ((args (method-parameters-to-lambda-parameters (pop rest))))
                `((lambda ,args ,@rest)))))))

(defun method-parameters-to-lambda-parameters (method-parameters)
  (let ((mp method-parameters))
    (append
     (loop until (or (not (consp mp))
                     (and (symbolp (car mp))
                          (member (car mp) lambda-list-keywords)))
          append (let ((m (pop mp)))
                   (typecase m
                     (symbol (list m))
                     (cons (if (symbolp (car m))
                               (list (car m))
                               nil)))))
     mp)))

(defun lambdas-of-defgeneric (expr)
  (and (proper-listp expr)
       (loop for x in (cdddr expr)
          when (and (consp x) (eql (car x) :method))
          append (lambdas-of-defmethod-body x))))

(defun lambdas-of-defmacro (expr)
  (and (proper-listp expr)
       (let ((macro-args (caddr expr))
             (body (cdddr expr)))
         (let ((lambda-args (macro-args-to-lambda-args macro-args)))
           (list `(lambda ,lambda-args ,@body))))))

(defun macro-args-to-lambda-args (macro-args)
  (labels ((%m (args)
             (cond
               ((null args) nil)
               ((member args '(&whole &rest &body &key &optional &allow-other-keys))
                nil)
               ((symbolp args) (list args))
               ((not (consp args)) nil)
               (t (append (%m (car args))
                          (%m (cdr args)))))))
    (remove-duplicates (%m macro-args))))

(defun remove-equal-dups (vals)
  (let ((table (make-hash-table :test 'equal)))
    (loop for e in vals
       append (unless (gethash e table)
                (setf (gethash e table) t)
                (list e)))))

(defun make-lambdas-from-files (&rest args)
  (setf *lambdas*
	(coerce (remove-equal-dups
                 (append (coerce *lambdas* 'list)
                         (apply #'lambdas-of-defs args)))
                'vector))
  #+sbcl (sb-ext:gc :full t)
  (length *lambdas*))

(defun make-lams-for-qlstems (system-stems &key (root "/pdietz/quicklisp/dists/quicklisp/software"))
  (loop for stem in system-stems
     do (let ((dir (format nil "~A/*~A*" root stem)))
          (format t "~A: ~A~%" stem (make-lams dir)))))

(defun lisp-paths-for-root-dir (root-dir)
  (loop for type in '("lisp" "lsp" "cl")
     collect (format nil "~A/**/*.~A" root-dir type)))

(defun make-lams (root-dir)
  (let ((dirs (lisp-paths-for-root-dir root-dir)))
    (make-lambdas-from-files dirs)))

(defvar *ql-dirs*
  '("~/quicklisp/**/*.lisp"
    "~/quicklisp/**/*.lsp"
    "~/quicklisp/**/*.cl"
    "~/quicklisp/**/*.l"))

(defun make-ql-lams ()
  (let ((dirs *ql-dirs*))
    (make-lambdas-from-files dirs)))

(defun make-sel-lams ()
  (let ((dirs '("~/quicklisp/local-projects/sel/**/*.lisp"
                "~/quicklisp/local-projects/sel/**/*.lsp"
                "~/quicklisp/local-projects/sel/**/*.cl")))
    (make-lambdas-from-files dirs)))

(defvar *ansi-test-dirs* '("~/ansi-test/**/*.lsp"))

(defvar *other-dirs* '("~/public-lisp/**/*.lisp"
                       "~/public-lisp/**/*.lsp"
                       "~/public-lisp/**/*.cl"
                       "~/public-lisp/**/*.l"
                       ))

(defun make-ansi-test-lams ()
  (let ((dirs *ansi-test-dirs*))
    (make-lambdas-from-files dirs)))

(defun make-all-lams ()
  (make-lambdas-from-files
   (append *ansi-test-dirs* *ql-dirs*
           *other-dirs*)))

(defun properize (x)
  (cons (loop while (consp x)
           collect (pop x))
        x))

(defun map-leafs (fn x)
  (loop while (consp x)
       do (map-leafs fn (pop x)))
  (funcall fn x))

(defun read-all-atoms-from-dir (root-dir)
  (let (#+nil(sb-impl::*use-sane-package-on-undefined-package* t))
    (let* ((dirs (lisp-paths-for-root-dir root-dir))
           (files (files-of-dirs dirs))
           (exprs (mapcan #'read-exprs-from-file files))
           (table (make-hash-table :test #'equal))
           (atoms nil))
      (loop for e in exprs
         unless (is-circular-tree? e)
         do (map-leafs
             (lambda (x)
               (unless (gethash x table)
                 (setf (gethash x table) t)
                 (push x atoms)))
             e))
      atoms)))

(defun make-qa-lams ()
  (let ((dirs '("~/quicklisp/**/*.lisp"
                "~/quicklisp/**/*.lsp"
                "~/quicklisp/**/*.cl"
                "~/quicklisp/**/*.l"
                "~/ansi-test/**/*.lsp")))
    (make-lambdas-from-files dirs)))

(defun make-ccl-lams ()
  (let ((dirs '("/pdietzccl/**/*.lisp")))
    (make-lambdas-from-files dirs)))

(defun make-acl2-lams ()
  (let ((dirs '("/pdietz/acl2-8.0/**/*.lisp")))
    (make-lambdas-from-files dirs)))

(defun make-random-from-lambdas (s)
  (declare (ignore s))
  (random-from-seq *lambdas*))

(defun remove-declares-from-tree (y)
  (if (consp y)
      (let ((front (loop while (consp y)
                      unless (and (consp (car y)) (eql (caar y) 'declare))
                      collect (remove-declares-from-tree (car y))
                      do (pop y))))
        (when y (setf (cdr (last front)) y))
        front)
      y))

(defun is-circular-tree? (root)
  (let ((table (make-hash-table)))
    (labels ((%traverse (x)
               (when (consp x)
                 (let ((y x))
                   (loop while (consp y)
                      do (when (gethash y table)
                           (return-from is-circular-tree? t))
                      do (setf (gethash y table) t)
                      do (%traverse (pop y))))
                 (loop while (consp x)
                    do (remhash x table)
                    do (pop x)))))
      (%traverse root)
      nil)))

(defun has-subtree-matching-pred (x pred)
  (labels ((%r (y)
             (or (funcall pred y)
                 (and (consp y)
                      (or (%r (car y))
                          (%r (cdr y)))))))
    (%r x)))

(defun has-duplicate-docstring? (root)
  (has-subtree-matching-pred root #'duplicate-docstring?))

(defun duplicate-docstring? (x)
  "True if the form at the root of x has a duplicate docstring.  This is used to filter
out forms that report useless error due to this."
  (and (consp x)
       (symbolp (car x))
       (case (car x)
         ((let let* lambda dotimes dolist) (body-has-duplicate-docstring? (cddr x)))
         ((multiple-value-bind destructuring-bind do do*)
          (body-has-duplicate-docstring? (cdddr x)))
         ((flet labels)
          (or (body-has-duplicate-docstring? (cddr x))
              (some #'(lambda (def) (body-has-duplicate-docstring? (cddr def))) (cadr x))))
         ((locally) (body-has-duplicate-docstring? (cdr x)))
         (t nil))))

(defun body-has-duplicate-docstring? (body)
  "True if the start of BODY as more than one docstring"
  (let ((count 0))
    (loop while (and (consp body)
                     (or (stringp (car body))
                         (and (consp (car body)) (eql (caar body) 'declare))))
       do (when (stringp (pop body)) (incf count)))
    (> count 1)))         

(defun has-prohibited-symbol (x)
  (has-subtree-matching-pred
   (cdr x)
   #'(lambda (y) (case y ((ldb
                           deposit-byte
                           mask-field
                           dpb
                           ldb
                           ldb-test
                           boole
                           unsigned-byte
                           signed-byte
                           invoke-debugger
                           load-time-value
                           #+sbcl sb-int:named-lambda
                           #+sbcl sb-sys:%primitive
                           #+sbcl sb-kernel:layout-of
                           #+sbcl sb-kernel:widetag-of
                           #+sbcl sb-kernel:%other-pointer-widetag
                           #+(and sbcl (not x86)) sb-vm::swap-complex
                           #+sbcl sb-alien:unsigned
                           #+sbcl sb-alien:make-alien
                           #+sbcl sb-alien:deref
                           #+sbcl sb-alien:cast
                           ash step symbol-macrolet
                           macrolet defun expt defmacro)
                          t)
                       (t nil)))))

(defun is-bad-mutated-form (x)
  (flet ((%is-bad (y)
           (and (consp y)
                (or
                 (and (eql (car y) 'aref)
                      (>= (length (cdr y)) 8))
                 (and (eql (car y) 'make-array)
                      (consp (cdr y))
                      (let ((d (second y)))
                        (and (consp d)
                             (eql (car d) 'quote)
                             (consp (second d))
                             (not (typep (car (second d)) '(integer 1))))))
                 (and (eql (car y) 'lambda)
                      (consp (cdr y))
                      (consp (cddr y))
                      (consp (cdddr y))
                      (stringp (third y))
                      (stringp (fourth y)))
                 ))))
    (has-subtree-matching-pred x #'%is-bad)))

(declaim (special *current-lambda-pos*))

(defun dead-code-test (&key (start 0))
  (let* ((lm *lambdas*)
         (len (length lm))
         )
    (loop for i from start below len do
         (let ((test-lambda
                `(lambda (x)
                   (cond
                     (x (,(elt lm i))
                        (the integer nil))
                     (t nil)))))
           (setf *current-lambda* test-lambda)
           (setf *current-lambda-pos* i)
           (handler-case
               (compile nil test-lambda))))))
             
(defun tree-find (val tree)
  (cond
    ((eql val tree) t)
    ((consp tree)
     (or (tree-find val (car tree))
         (tree-find val (cdr tree))))))

(defun tree-findf (val)
  (lambda (tree) (tree-find val tree)))
