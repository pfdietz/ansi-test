# LISP=gcl
# LISP=sbcl --noinform
# LISP=~/sbcl/src/runtime/sbcl --core ~/sbcl/output/sbcl.core --noinform
# LISP=clisp -ansi -q
# LISP=abcl
# LISP=ecl

MAKE=make

test:
	@rm -rf scratch
	cat doit.lsp | $(LISP) | tee test.out

test-compiled:
	@rm -rf scratch
	echo "(load \"compileit.lsp\")" | $(LISP) | tee test.out

test-unixport:
	echo "(load \"doit.lsp\")" | ../unixport/saved_ansi_gcl | tee test.out

random-test:
	(echo "(progn #+gcl (setq compiler::*cc* \"gcc -c -DVOL=volatile -fsigned-char -pipe \") \
		(setq *load-verbose* nil) \
		(let* ((*standard-output* (make-broadcast-stream)) \
		     (*error-output* *standard-output*)) \
		(load \"gclload1.lsp\") \
		(funcall (symbol-function 'compile-and-load) \"random-int-form.lsp\")))  \
	      (in-package :cl-test) \
	      (let ((x (cl-test::test-random-integer-forms 1000 3 1000 :random-size t :random-nvars t))) \
		(setq x (cl-test::prune-results x)) \
		(with-open-file (*standard-output* \"failures.lsp\" \
		   :direction :output \
		   :if-exists :append \
		   :if-does-not-exist :create) \
		  (mapc #'print x))) \
                #+allegro (excl::exit) \
		; extra quits added to avoid being trapped in debugger in some lisps \
	        (cl-user::quit) \
                (cl-user::quit) \
                (cl-user::quit) \
                (cl-user::quit) \
                (cl-user::quit) \
                (cl-user::quit) \
                (cl-user::quit) \
                (cl-user::quit) \
                (cl-user::quit) \
                (cl-user::quit) \
                (cl-user::quit)") | $(LISP)
	rm -f gazonk*

rt_1000_8:
		echo "(load \"gclload1.lsp\") \
		(compile-and-load \"random-int-form.lsp\")  \
		(in-package :cl-test) (loop-random-int-forms 1000 8)" | $(LISP)


clean:
	@rm -f test.out *.cls *.fasl *.o *.so *~ *.fn *.x86f *.fasl *.ufsl *.abcl *.fas *.lib \#*\#
	@(cd beyond-ansi; $(MAKE) clean)
	@rm -rf scratch/ scratch.txt
	@rm -f foo.txt foo.lsp foo.dat
	@rm -f tmp.txt tmp.dat tmp2.dat temp.dat
	@rm -f gazonk* out.class
	@rm -rf TMP/
	@rm -f "CLTEST:file-that-was-renamed.txt" file-that-was-renamed.txt

