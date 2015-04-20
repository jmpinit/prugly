all:
	gcc -c -Wall -Werror -fpic simpledrv.c
	gcc -shared -lprussdrv -o libsimpledrv.so simpledrv.o

.PHONY: all
