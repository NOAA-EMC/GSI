# *****************************************************************
#  makefile
#
#  Make the validate_time.x executable.  This is not normally needed
#  for normal use.  It re-validates data in an extracted *.ieee_d
#  file, applying a different set of historic base files in the
#  validation. 
#
#  If needed, run make on this directory and move the executable
#  over to the data_extract/exec directory.
#
# *****************************************************************

BINDIR = ../../exec

LIBS   = $(W3NCO_LIB4)

OBJS   = kinds.o bad_obs.o bad_penalty.o bad_chan.o valid.o \
	 validate_time.o

#
# *****************************************************************
#

CMD = validate_time.x

all:  $(CMD)

clean:
	rm -f *.o
	rm -f *.mod
	rm -f validate_time.x 

check_prereqs:
	/nwprod/spa_util/check_libs.bash $(LIBS)

install:
	cp -f $(CMD) $(BINDIR)/

debug:  FFLAGS = $(D_FFLAGS)
debug:  $(CMD)

$(CMD):         $(OBJS)
	$(CF) $(FFLAGS) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

bad_obs.o : bad_obs.f90
	$(CF) $(FFLAGS) -c $(*).f90

bad_penalty.o : bad_penalty.f90
	$(CF) $(FFLAGS) -c $(*).f90

bad_chan.o : bad_chan.f90
	$(CF) $(FFLAGS) -c $(*).f90

valid.o : valid.f90
	$(CF) $(FFLAGS) -c $(*).f90

validate_time.o : validate_time.f90
	$(CF) $(FFLAGS) -c $(*).f90

kinds.o : kinds.F90
	$(CF) $(FFLAGS) -c $(*).F90
