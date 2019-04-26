# *****************************************************************
#  makefile
#
#  Make the make_base executable.  The executable will construct
#  base history files for each entry in the SATYPE list from the 
#  time data files (*.ieee_d) for the specified source (suffix).
#
#  This is not normally necessary for RadMon users unless you want
#  to use the automated validation mechanism.
# *****************************************************************

BINDIR = ../../exec

LIBS   = $(W3NCO_LIB4)

OBJS   =  make_base.o

#
# *****************************************************************
#

CMD = make_base

all:  $(CMD)

clean:
	rm -f *.o
	rm -f *.mod
	rm -f make_base

check_prereqs:
	/nwprod/spa_util/check_libs.bash $(LIBS)

install:	
	cp -f $(CMD) $(BINDIR)/.

debug:  FFLAGS = $(D_FFLAGS)
debug:  $(CMD)

$(CMD):         $(OBJS)
	$(CF) $(FFLAGS) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

make_base.o : make_base.f90
	$(CF) $(FFLAGS) -c $(*).f90
