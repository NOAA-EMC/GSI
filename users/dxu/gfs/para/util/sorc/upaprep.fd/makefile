SHELL=/bin/sh
#

SRCS=   upaupa.f forupa.f redupa.f redhgt.f fndhgt.f brdhgt.f \
        conaft.f contrl.f dosbyt.f foraft.f conwnd.f forwnd.f \
        consat.f forsat.f

OBJS=   upaupa.o forupa.o redupa.o redhgt.o fndhgt.o brdhgt.o \
        conaft.o contrl.o dosbyt.o foraft.o conwnd.o forwnd.o \
        consat.o forsat.o

# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
# PROFLIB	Library needed for profiling
#
FC =		$(FCMP)
LDFLAGS =       -lessl
LIBS =          -L$(LIBDIR) -lw3_8 -lgph_8 -lip_8 -lsp_8 \
                -lbacio_8 -lgphcntr_8 -lbufr_8_64
CMD =		upaprep        
PROFLIB =	-lprof

# To perform the default compilation, use the first line
# To compile with flowtracing turned on, use the second line
# To compile giving profile additonal information, use the third line
# WARNING:  SIMULTANEOUSLY PROFILING AND FLOWTRACING IS NOT RECOMMENDED 
FFLAGS =         -qintsize=8 -qrealsize=8

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS) $(BUFR)

# Make the profiled version of the command and call it a.out.prof
#
$(CMD).prof:	$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(PROFLIB) $(LIBS)

clean:
	-rm -f $(OBJS)

clobber:	clean
	-rm -f $(CMD) $(CMD).prof

void:	clobber
	-rm -f $(SRCS) makefile
