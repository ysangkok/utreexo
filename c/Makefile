all: main

libforest.a: forest.c forest.h ../accumulator/*.go
	gcc -fPIC -fsanitize=address -ggdb3 -Wall -c -o libforest.o forest.c && ar rcs libforest.a libforest.o

libutreexo.h: forest_c_.go libforest.a
	CGO_LDFLAGS=-lasan CGO_CPPFLAGS="-ggdb3 -fPIC" go build -buildmode c-shared -o ./libutreexo.so forest_c_.go

main: main.c libutreexo.h
	gcc -lasan -ggdb3 -Wall -o main main.c -lutreexo -L./ -Wl,-rpath=.

.PHONY: run clean haskell

run: main
	./main

clean:
	rm -rf *.a *.o *.so main libutreexo.h Forest_hsc_make.c dist-newstyle

haskell: libutreexo.h updateDirt.json
	LD_LIBRARY_PATH=. cabal run utreexo -- --color always

updateDirt.txt:
	cd ..; go test ./accumulator -test.v | grep updateDirt > c/updateDirt.txt

updateDirt.json: updateDirt.txt
	bash -c '(echo "["; cut -d" " -f2 < updateDirt.txt | sed -re "s/$$/,/" | sed -re "s/null/[]/g" | head -c-2; echo "]")  | jq > updateDirt.json'
