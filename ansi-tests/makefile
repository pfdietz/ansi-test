# LISP=gcl

test:
	@rm -rf scratch
	echo "(load \"doit.lsp\")" | $(LISP) | tee test.out

test-compiled:
	@rm -rf scratch
	echo "(load \"compileit.lsp\")" | $(LISP) | tee test.out

test-unixport:
	echo "(load \"doit.lsp\")" | ../unixport/saved_ansi_gcl | tee test.out

random-test:
	echo "(progn (setq *load-verbose* nil) \
		(let* ((*standard-output* (make-broadcast-stream)) \
		     (*error-output* *standard-output*)) \
		(load \"gclload1.lsp\") \
		(funcall (symbol-function 'compile-and-load) \"random-int-form.lsp\")))  \
	      (in-package :cl-test) \
	      (let ((x (cl-test::test-random-integer-forms 1000 10 100 :random-size t :random-nvars t))) \
		(setq x (cl-test::prune-results x)) \
		(with-open-file (*standard-output* \"failures.lsp\" \
		   :direction :output \
		   :if-exists :append \
		   :if-does-not-exist :create) \
		  (mapc #'print x)) \
		#-allegro (quit) #+allegro (excl::exit)))" | $(LISP)
	rm -f gazonk*

clean:
	@rm -f test.out *.cls *.fasl *.o *.so *~ *.fn *.x86f *.fasl *.ufsl *.abcl *.fas *.lib \#*\#
	@rm -rf scratch/ scratch.txt
	@rm -f foo.txt foo.lsp foo.dat
	@rm -f tmp.txt tmp.dat tmp2.dat temp.dat
	@rm -f gazonk* out.class
	@rm -rf TMP/
	@rm -f "CLTEST:file-that-was-renamed.txt" file-that-was-renamed.txt

