# Makefile for running the Lisp interpreter

# Variable for the Lisp interpreter file
LISP_FILE = gpp_interpreter.lisp
INPUT_FILE = input.gpp

# Default target
all: compile

# Compile target: Runs the Lisp interpreter without input.gpp
compile:
	clisp $(LISP_FILE)

# run1 target: Runs the Lisp interpreter without any input file
run1: 
	clisp $(LISP_FILE)

# run2 target: Runs the Lisp interpreter with input.gpp as a parameter
run2:
	clisp $(LISP_FILE) $(INPUT_FILE)
