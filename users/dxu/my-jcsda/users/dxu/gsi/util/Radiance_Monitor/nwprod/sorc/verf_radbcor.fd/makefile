# *****************************************************************
#  makefile.bcor
#
#  Make the radmon_bcor.glb executable.  The executable will extract
#  bcor radiance data from a diagnostic file from a global run.
#
# *****************************************************************

SHELL=/bin/sh
SRCS = kinds.F90 read_diag.f90 bcor.f90 avgsdv.f90 create_ctl_bcor.f90

OBJS =  kinds.o read_diag.o bcor.o avgsdv.o create_ctl_bcor.o

#
# *****************************************************************
#

#------------
# Include machine dependent compile & load options
#------------
MAKE_CONF = Makefile.conf
include $(MAKE_CONF)


CMD = radmon_bcor

all:  $(CMD)

clean:
	rm -f *.o
	rm -f *.mod
	rm -f radmon_bcor

$(CMD):         $(OBJS)
	$(CF) $(FFLAGS) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

read_diag.o : read_diag.f90
	$(CF) $(FFLAGS) -c $(*).f90

bcor.o : bcor.f90
	$(CF) $(FFLAGS) -c $(*).f90

avgsdv.o : avgsdv.f90
	$(CF) $(FFLAGS) -c $(*).f90

create_ctl_bcor.o : create_ctl_bcor.f90
	$(CF) $(FFLAGS) -c $(*).f90

kinds.o : kinds.F90
	$(CF) $(FFLAGS) -c $(*).F90
