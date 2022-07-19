main: main.c
	$(CC) -Wall main.c mpc.c -lm -pedantic -std=c99 -o main

clean:
	rm -rf main

run:
	./main
