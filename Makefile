SHELL := /bin/bash

.PHONY: clean all vv

vv: 
	cd src/ && rm -rf Parser && rm -f Makefile && bnfc -m ../VVSyntax.cf -p Parser && make
	stack build
	stack install --local-bin-path ./
	chmod +x ./vv-exe
	ln -s ./vv-exe ./interpreter 

all: vv

clean:
	rm -f ./interpreter
	rm -f ./vv-exe
	cd src/ && rm -rf Parser && rm -f Makefile
