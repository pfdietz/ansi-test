
test:
	echo "(load \"gclload.lsp\")" | gcl | tee test.out

test-unixport:
	echo "(load \"gclload.lsp\")" | ../unixport/saved_ansi_gcl | tee test.out

clean:
	rm -f test.out *.fasl *.o *~ *.fn *.x86f *.fasl rt/*.o rt/*.fasl rt/*.fn rt/*~ rt/*.x86f rt/*.fasl
