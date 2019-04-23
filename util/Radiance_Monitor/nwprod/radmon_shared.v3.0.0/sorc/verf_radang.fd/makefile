# *****************************************************************
#  makefile.angle_bias
#
#  Make the radmon_angle executable. 
#
# *****************************************************************

#BINDIR = $(dir_root)/exec
BINDIR = ../../exec

LIBS   = $(W3NCO_LIB4) \
	${NETCDF_LIBRARIES_F90} \
	${NETCDF_LIBRARIES} \
	${HDF5_Fortran_HL_LIBRARIES} \
	${MPI_Fortran_LIBRARIES} -L./ ${W3NCO_4_LIBRARY} ${HDF5_LIBRARIES} ${NCDIAG_LIBRARIES} ) \
	${CRAY_MPICH}/lib

OBJS   = kinds.o mpeu_mpif.o mpimod.o \
	sparsearr.o stop1.o read_diag.o angle_bias.o create_ctl_angle.o 

#
# *****************************************************************
#

CMD = radmon_angle.x

all:  $(CMD)

clean:
	rm -f *.o
	rm -f *.mod
	rm -f $(CMD)

check_prereqs:
	$(PRE_REQ) $(LIBS)

install:
	cp -f $(CMD) $(BINDIR)/

debug:	FFLAGS = $(D_FFLAGS) 
debug:	$(CMD)

$(CMD):         $(OBJS)
	$(CF) $(FFLAGS) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

kinds.o : kinds.F90
	$(CF) $(FFLAGS) -c $(*).F90

mpeu_mpif.o : mpeu_mpif.F90
	$(CF) $(FFLAGS) -c $(*).F90

mpimod.o : mpimod.F90
	$(CF) $(FFLAGS) -c $(*).F90

sparsearr.o : sparsearr.f90
	$(CF) $(FFLAGS) -c $(*).F90

stop1.o : stop1.f90
	$(CF) $(FFLAGS) -c $(*).F90

read_diag.o : read_diag.f90
	$(CF) $(FFLAGS) -c $(*).f90

angle_bias.o : angle_bias.f90
	$(CF) $(FFLAGS) -c $(*).f90

create_ctl_angle.o : create_ctl_angle.f90
	$(CF) $(FFLAGS) -c $(*).f90

