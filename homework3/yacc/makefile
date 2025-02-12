CC = gcc
LEX = flex
YACC = yacc

# Derleme komutları
all: gpp_interpreter

gpp_interpreter: y.tab.c lex.yy.c
	$(CC) -o gpp_interpreter y.tab.c lex.yy.c -lfl

y.tab.c: gpp_interpreter.y
	$(YACC) -d gpp_interpreter.y

lex.yy.c: gpp_interpreter.l
	$(LEX) gpp_interpreter.l


run1: gpp_interpreter
	./gpp_interpreter


run2: gpp_interpreter
	./gpp_interpreter input.txt

clean:
	rm -f gpp_interpreter y.tab.c lex.yy.c y.tab.h
