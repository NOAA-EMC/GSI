# *****************************************************************
#
#  makefile
#
#  Make the angle_bias.rgn executable.  This will extract 
#  angle radiance data from an NDAS diagnostic file.
#
# *****************************************************************

SHELL=/bin/sh
SRCS = kinds.F90 read_diag.f90 angle_bias.f90 \
        create_ctl_angle.f90

OBJS = kinds.o read_diag.o angle_bias.o \
	create_ctl_angle.o 

#
# *****************************************************************
#

#------------
# Include machine dependent compile & load options
#------------
MAKE_CONF = Makefile.conf
include $(MAKE_CONF)


CMD = radmon_angle.rgn

all:  $(CMD)

clean:
	rm -f *.o
	rm -f *.mod
	rm -f *.rgn

$(CMD):         $(OBJS)
	$(CF) $(FFLAGS) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

read_diag.o : read_diag.f90
	$(CF) $(FFLAGS) -c $(*).f90

angle_bias.o : angle_bias.f90
	$(CF) $(FFLAGS) -c $(*).f90

create_ctl_angle.o : create_ctl_angle.f90
	$(CF) $(FFLAGS) -c $(*).f90

kinds.o : kinds.F90
	$(CF) $(FFLAGS) -c $(*).F90
