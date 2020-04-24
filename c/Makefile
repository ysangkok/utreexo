all: main

libforest.a: forest.c forest.h ../accumulator/*.go
	gcc -Wall -c -o libforest.o forest.c && ar rcs libforest.a libforest.o

libutreexo.h: forest_c_.go libforest.a
	go build -buildmode c-shared -o ./libutreexo.so forest_c_.go

main: main.c libutreexo.h
	gcc -Wall -o main main.c -lutreexo -L./ -Wl,-rpath=.

.PHONY: run clean haskell

run: main
	./main

clean:
	rm -rf *.a *.o *.so main libutreexo.h Forest_hsc_make.c dist-newstyle

haskell: libutreexo.h
	LD_LIBRARY_PATH=. cabal run utreexo -- --color always
