.PHONY: all
all: lexer.cpp

lexer.cpp: lexer.ll
	flex -o $@ $^

.PHONY: clean
clean:
	rm lexer.cpp 
