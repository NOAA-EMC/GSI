SHELL=/bin/sh

LIB = ../../../lib/libgsdcloud.a

include make.filelist

.SUFFIXES: .f90 .o

.f90.o:
##	$(RM) $@ $*.mod
	$(F90) $(FFLAGS) -c $<


all:	$(LIB)

$(LIB):	$(OBJ_FILES)
	$(AR) -ruv $(LIB) $(OBJ_FILES)

.f90.a:
	$(F90) -c $(FFLAGS) $<
#	ar -ruv $(AFLAGS) $@ $*.o
#	rm -f $*.o

.c.a:
	$(CC) -c $(CFLAGS) $<
#	ar -ruv  $(AFLAGS) $@ $*.o
#	rm -f $*.o

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

include make.dependencies

clean:
	rm -f *.o *.mod $(LIB)

