LISP=gcl
ACL=~/acl62_trial/alisp

test:
	rmdir scratch
	echo "(load \"gclload.lsp\")" | $(LISP) | tee test.out

test-unixport:
	echo "(load \"gclload.lsp\")" | ../unixport/saved_ansi_gcl | tee test.out

random-test:
	echo "(load \"gclload1.lsp\") \
		(compile-and-load \"random-int-form.lsp\")  \
		(in-package :cl-test) \
		(let ((x (cl-test::test-random-integer-forms 1000 3 200))) \
		  (print (cl-test::prune-results x)) nil)" | $(LISP)
random-acl-test:
	echo "(progn (setq *load-verbose* nil) (load \"gclload1.lsp\")) \
	      (progn \
		(compile-and-load \"random-int-form.lsp\")  \
		(in-package :cl-test) \
		(setq cl-test::*compile-unoptimized-form* nil) \
		(let ((x (cl-test::test-random-integer-forms 1000 3 50))) \
		  (print (cl-test::prune-results x)) nil))" | $(ACL)


clean:
	rm -f test.out *.fasl *.o *.so *~ *.fn *.x86f *.fasl *.ufsl *.fas *.lib \#*\#; rm -rf scratch/; rm -f foo.txt foo.lsp file-that-was-renamed.txt tmp.dat temp.dat
