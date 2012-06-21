## basic GNU Makefile for DFSIM

DFSIM_OBJS = 	main.o \
		functions.o \
		subroutines.o \
		econ.o

## these are general compiler directives
CC		= gcc 
FC		= gfortran
##CFLAGS		= -g -Wall
FFLAGS		= -O3

################################################################################
## make target for the dfsim application
dfsim: ${DFSIM_OBJS} ${LIB}
	${FC} -o $@ ${DFSIM_OBJS} ${LIB} 
simin:
	${FC} -o simin simin.for

main.o: 
	${FC} -c main.for

functions.o: 
	${FC} -c functions.for

subroutines.o: 
	${FC} -c subroutines.for

econ.o: 
	${FC} -c econ.for


.PHONY : clean

################################################################################
## clean up the temp files
clean:  ${DFSIM_OBJS}
	rm -fv *.o

