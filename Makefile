main: main.c
	$(CC) -Wall main.c external/mpc.c -g -lm -std=c99 -o main

clean:
	rm -rf main

run:
	./main
