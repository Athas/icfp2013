all: genetic

debug: genetic_debug

genetic: genetic.c data.h
	gcc -O3 -march=native -mtune=native -fomit-frame-pointer -fwhole-program -fshort-enums genetic.c -o genetic

genetic_debug: genetic.c data.h
	gcc -O3 -march=native -mtune=native -fomit-frame-pointer -fwhole-program -fshort-enums genetic.c -o genetic_debug
