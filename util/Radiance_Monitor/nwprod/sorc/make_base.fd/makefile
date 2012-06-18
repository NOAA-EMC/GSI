# *****************************************************************
#  makefile
#
#  Make the make_base executable.  The executable will construct
#  base history files for each entry in the SATYPE list from the 
#  time data files (*.ieee_d) for the specified source (suffix).
#
# *****************************************************************

SHELL=/bin/sh
SRCS = make_base.f90

OBJS =  make_base.o

#
# *****************************************************************
#

CF=xlf90

LIBS    = -L/nwprod/lib -lw3_4
FFLAGS= -qsuffix=f=f90 -qdbg -qfullpath -O2 -qmaxmem=-1

LDFLAGS=

CMD = make_base

all:  $(CMD)

clean:
	rm -f *.o
	rm -f *.mod
	rm -f make_base

$(CMD):         $(OBJS)
	$(CF) $(FFLAGS) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

make_base.o : make_base.f90
	$(CF) $(FFLAGS) -c $(*).f90
