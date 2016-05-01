.PHONY: programs all clean distclean
CFLAGS = --std=gnu11 -Wall -Wextra -Wpedantic -g
CC = gcc $(CFLAGS)
ASSEMBLE = ./assemble
SAMPLE_PROGS = sum.sextium halt.sextium gcd.sextium

sextium: portable_endian.o sexproc.o sextium.c sexproc.h sextium.c
	$(CC) -o sextium portable_endian.o sexproc.o sextium.c

sexproc.o: sexproc.c sexproc.h
	$(CC) -Wno-type-limits -c sexproc.c

portable_endian.o: portable_endian.h
	$(CC) -c -x c portable_endian.h

$(SAMPLE_PROGS): %.sextium: %.sasm
	$(ASSEMBLE) $< $@ 2>/dev/null

programs: $(SAMPLE_PROGS)

all: sextium programs

clean:
	rm -f *.o

distclean: clean
	rm -f sextium *.sextium
