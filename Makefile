# Project: 		Graph Spanning Tree
# Decription:	Obtain all spanning trees of a graph.
# File: 		Makefile
# Version: 		1.0
# Course: 		FLP - Functional and Logic Programming
# Organisation:	Brno University of Technology - Faculty of Information Technology
# Author: 		Daniel Konecny (xkonec75)
# Date: 		25. 04. 2021

# Macros
PROLOG = swipl
SUFFIX = pl
PROJECT = flp20-log
LOGIN = xkonec75
SRC = src/
DOC = doc/
TEST = test/

all: $(PROJECT)

test:
	./$(PROJECT) < $(TEST)test$(NUM).in > $(TEST)test$(NUM).tmp

clean:
	rm $(PROJECT) $(TEST)*.tmp

pack:
	zip -r flp-log-$(LOGIN).zip $(SRC) $(DOC) $(TEST) Makefile

# Binary
$(PROJECT): $(SRC)main.$(SUFFIX) $(SRC)input.$(SUFFIX) $(SRC)graph.$(SUFFIX)
	$(PROLOG) -q -g main -o $@ -c $^
