# LISP=gcl
# ACL=~/acl62_trial/alisp

test:
	@rm -rf scratch
	echo "(load \"doit.lsp\")" | $(LISP) | tee test.out

test-compiled:
	@rm -rf scratch
	echo "(load \"compileit.lsp\")" | $(LISP) | tee test.out

test-unixport:
	echo "(load \"doit.lsp\")" | ../unixport/saved_ansi_gcl | tee test.out

random-test:
	echo "(load \"gclload1.lsp\") \
		(compile-and-load \"random-int-form.lsp\")  \
		(in-package :cl-test) \
		(let ((x (cl-test::test-random-integer-forms 1000 6 100))) \
		  (setq x (cl-test::prune-results x)) \
		  (with-open-file (*standard-output* \"failures.lsp\" \
			 :direction :output \
			 :if-exists :append \
			 :if-does-not-exist :create) \
		      (mapc #'print x)) \
		   (quit)))" | $(LISP)

random-acl-test:
	echo "(progn (setq *load-verbose* nil) (load \"gclload1.lsp\")) \
	      (progn \
		(compile-and-load \"random-int-form.lsp\")  \
		(in-package :cl-test) \
		(setq cl-test::*compile-unoptimized-form* nil) \
		(let ((x (cl-test::test-random-integer-forms 1000 3 1000))) \
		  (setq x (cl-test::prune-results x)) \
		  (with-open-file (*standard-output* \"failures.lsp\" \
			 :direction :output \
			 :if-exists :append \
			 :if-does-not-exist :create) \
		      (mapc #'print x)) \
		   (exit)))" | $(ACL)

clean:
	rm -f test.out *.cls *.fasl *.o *.so *~ *.fn *.x86f *.fasl *.ufsl *.fas *.lib \#*\#; rm -rf scratch/; rm -f foo.txt foo.lsp file-that-was-renamed.txt tmp.dat temp.dat
