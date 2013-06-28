# *****************************************************************
#  makefile
#
#  Make the time.rgn executable.  This will extract time 
#  radiance data from a diagnostic file from a regional run.
#
# *****************************************************************

SHELL=/bin/sh
SRCS = kinds.F90 read_diag.f90 time.f90 create_ctl_time.f90

OBJS = kinds.o read_diag.o time.o create_ctl_time.o

#
# *****************************************************************
#

#------------
# Include machine dependent compile & load options
#------------
MAKE_CONF = Makefile.conf
include $(MAKE_CONF)


CMD = radmon_time.rgn

all:  $(CMD)

clean:
	rm -f *.o
	rm -f *.mod
	rm -f *.rgn

$(CMD):         $(OBJS)
	$(CF) $(FFLAGS) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

read_diag.o : read_diag.f90
	$(CF) $(FFLAGS) -c $(*).f90

time.o : time.f90
	$(CF) $(FFLAGS) -c $(*).f90

create_ctl_time.o : create_ctl_time.f90
	$(CF) $(FFLAGS) -c $(*).f90

kinds.o : kinds.F90
	$(CF) $(FFLAGS) -c $(*).F90
