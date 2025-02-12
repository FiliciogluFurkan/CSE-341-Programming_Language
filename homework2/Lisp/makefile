# Makefile for GPP Lexer


LISP = sbcl


TEST_FILE = input.txt


run2:
	$(LISP) --load gpp_lexer.lisp --eval "(gppinterpreter)" --eval "(quit)"


run3:
	$(LISP) --load gpp_lexer.lisp --eval "(gppinterpreter \"$(TEST_FILE)\")" --eval "(quit)"


clean:
	rm -f *.fasl


all: run2

.PHONY: run2 run3 clean all