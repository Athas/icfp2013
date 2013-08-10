all: genetic

debug: genetic_debug

genetic: genetic.c data.h
	gcc -Wall -O3 -march=native -mtune=native -fomit-frame-pointer -fwhole-program -fshort-enums genetic.c -o genetic

genetic_debug: genetic.c data.h
	gcc -Wall -O0 -ggdb genetic.c -o genetic_debug

clean:
	rm -f *.hi
	rm -f *.o
	rm -f genetic
	rm -rf genetic genetic_debug

