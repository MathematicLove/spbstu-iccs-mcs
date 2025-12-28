CC = gcc
CFLAGS = -Wall -Wextra -O2
OBJ = main.o server.o daemon.o

chatserver: $(OBJ)
	$(CC) $(CFLAGS) -o $@ $(OBJ)

%.o: %.c
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f *.o chatserver

.PHONY: clean


