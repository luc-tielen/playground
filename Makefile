build:
	g++ -O0 src/main.c -o program

run: build
	./program

clean:
	rm program

.PHONY: build run clean
