CC=mpif90



%.o: %.f90 
	$(CC) -c -o $@ $< $(CFLAGS) 
 

isendmake: isend.o 
	$(CC)  -o isend isend.o -I -O3

.PHONY: clean
clean:
	rm *.o  isend
