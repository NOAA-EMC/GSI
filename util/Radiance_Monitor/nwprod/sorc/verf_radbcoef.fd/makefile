# *****************************************************************
#  makefile.bcoef
#
#  Make the radmon_bcoef.glb executable.  The executable will extract
#  bcoef radiance data from a diagnostic file from a global run.
#
# *****************************************************************

SHELL=/bin/sh
SRCS = kinds.F90 read_diag.f90 bcoef.f90 create_ctl_bcoef.f90

OBJS = kinds.o read_diag.o bcoef.o create_ctl_bcoef.o

#
# *****************************************************************
#

#------------
# Include machine dependent compile & load options
#------------
MAKE_CONF = Makefile.conf
include $(MAKE_CONF)



CMD = radmon_bcoef

all:  $(CMD)

clean:
	rm -f *.o
	rm -f *.mod
	rm -f radmon_bcoef 

$(CMD):         $(OBJS)
	$(CF) $(FFLAGS) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

read_diag.o : read_diag.f90
	$(CF) $(FFLAGS) -c $(*).f90

bcoef.o : bcoef.f90
	$(CF) $(FFLAGS) -c $(*).f90

create_ctl_bcoef.o : create_ctl_bcoef.f90
	$(CF) $(FFLAGS) -c $(*).f90

kinds.o : kinds.F90
	$(CF) $(FFLAGS) -c $(*).F90
