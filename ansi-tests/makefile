
test:
	echo "(load \"gclload.lsp\")" | gcl | tee test.out

test-unixport:
	echo "(load \"gclload.lsp\")" | ../unixport/saved_ansi_gcl | tee test.out

clean:
	rm -f test.out *.fasl *.o *.so *~ *.fn *.x86f *.fasl *.ufsl *.fas *.lib \#*\#
