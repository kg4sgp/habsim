CFLAGS=-Wall -g -lm

default: habsim

habsim.o: habsim.c
	$(CC) -c habsim.c -o habsim.o $(CFLAGS)

habsim: habsim.o
	$(CC) habsim.o -o habsim $(CFLAGS)

clean:
	-rm -v habsim.o habsim
