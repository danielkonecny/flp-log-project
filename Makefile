# Project: 		Graph Spanning Tree
# Decription:	Obtain all spanning trees of a graph.
# File: 		Makefile
# Version: 		1.0
# Course: 		FLP - Functional and Logic Programming
# Organisation:	Brno University of Technology - Faculty of Information Technology
# Author: 		Daniel Konecny (xkonec75)
# Date: 		21. 04. 2021

# Macros
PROLOG = swipl
SUFFIX = pl
PROJECT = flp20-log
LOGIN = xkonec75
SRC = src/
DOC = doc/
TEST = test/

all: $(PROJECT)

test: $(PROJECT)
	./$(PROJECT) < $(TEST)test1.in > $(TEST)test1.tmp

clean:
	rm $(PROJECT) $(TEST)*.tmp

pack:
	zip -r flp-log-$(LOGIN).zip $(SRC) $(DOC) $(TEST) Makefile

# Binary
$(PROJECT): $(SRC)main.$(SUFFIX) $(SRC)input.$(SUFFIX) 
	$(PROLOG) -q -g main -o $@ -c $^
