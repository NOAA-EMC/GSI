SHELL=	/bin/sh
FCOMP  = ifort
FFLAGS = -check all -traceback -fpe0 -ftrapuv -g
EXE= fov_util.exe

OBJS = driver.o \
       calc_fov_conical.o \
       calc_fov_crosstrk.o \
       kinds.o \
       constants.o 

$(EXE):	$(OBJS)
	$(FCOMP) $(FFLAGS) -o $(EXE) $(OBJS) 

driver.o: calc_fov_crosstrk.o calc_fov_conical.o kinds.o constants.o driver.f90
	$(FCOMP) $(FFLAGS) -c driver.f90
calc_fov_conical.o: kinds.o constants.o calc_fov_conical.f90 
	$(FCOMP) $(FFLAGS) -c calc_fov_conical.f90
calc_fov_crosstrk.o: kinds.o constants.o calc_fov_crosstrk.f90 
	$(FCOMP) $(FFLAGS) -c calc_fov_crosstrk.f90
kinds.o:   kinds.F90
	$(FCOMP) $(FFLAGS) -D_REAL8_ -c kinds.F90
constants.o:  kinds.o constants.f90
	$(FCOMP) $(FFLAGS) -c constants.f90
clean:
	rm -f *.o *.mod $(EXE)
