CFLAGS=-Wall -g -lm -pedantic -std=c99

default: habsim

habsim.o: habsim.c
	$(CC) -c habsim.c -o habsim.o $(CFLAGS)

habsim: habsim.o
	$(CC) habsim.o -o habsim $(CFLAGS)

clean:
	-rm -v habsim.o habsim
