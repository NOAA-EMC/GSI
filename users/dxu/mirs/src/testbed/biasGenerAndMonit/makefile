SHELL = /bin/sh

include ../../../setup/paths

MAIN_path     = .
CURR_path     = `pwd`

#---Default macros and rules (from CRTM)
include ${CRTM_path}/make.macros
include ${CRTM_path}/make.rules

# -------------
# This makefile
# -------------
MAKEFILE = makefile

# ----------------
# Executable
# ----------------
EXE_1 = ${exec_path}/Calib_generic_rad
EXE_2 = ${exec_path}/DeterminBiasCorr

EXE_S = ${EXE_1} ${EXE_2}

# ------------
# Object files
# ------------
OBJ_MAIN_1 =  ${MAIN_path}/Calib_generic_rad.o
OBJ_MAIN_2 =  ${MAIN_path}/DeterminBiasCorr.o

OBJ_MAIN_S =  $(OBJ_MAIN_1) $(OBJ_MAIN_2)

OBJ_LIB    =  $(lib_path)/misc/Consts.o \
              ${lib_path}/misc/misc.o \
              ${lib_path}/io/IO_MeasurData.o \
              ${lib_path}/io/IO_Noise.o \
              ${lib_path}/io/IO_Misc.o \
              ${lib_path}/io/IO_Scene.o \
              $(lib_path)/qc/ErrorHandling.o \
              $(lib_path)/utilities/utils.o \
              $(lib_path)/math/mathFcts.o \
              $(lib_path)/noise/Noise.o
               		     

OBJ_1      =  ${OBJ_LIB} ${OBJ_MAIN_1} lsq.o
OBJ_2      =  ${OBJ_LIB} ${OBJ_MAIN_2}

SRC_1      = Calib_generic_rad.f90
SRC_2      = DeterminBiasCorr.f90

SRC_LIB    = ${OBJ_LIB:.o=.f90}

# -------------------------------
# Include and library definitions
# -------------------------------
INCLUDES  =  -I$(lib_path)/misc \
             -I$(lib_path)/io \
             -I$(lib_path)/qc \
             -I$(lib_path)/utilities \
             -I$(lib_path)/math \
	     -I$(lib_path)/noise -I./

LIBRARIES =

# -----------------------
# Extra compilation flags
# -----------------------
EXTRA_FC_FLAGS = $(INCLUDES) $(convEndOption)


# ---------------------
# Make based on OS type
# ---------------------

all:
	@echo "OS type detected: "`uname -s`
	@case `uname -s` in \
		"SunOS")   make -f $(MAKEFILE) the_program $(SUNOS_FLAGS)  ;; \
		"AIX")     make -f $(MAKEFILE) the_program $(AIX_FLAGS)    ;; \
		"IRIX64" ) make -f $(MAKEFILE) the_program $(IRIX64_FLAGS) ;; \
		"Linux" )  make -f $(MAKEFILE) the_program $(LINUX_FLAGS)  ;; \
		*) echo "This system is not supported"                     ;; \
	esac


# ---------------------
# Make the test program
# ---------------------
the_program : $(EXE_S)


# ---------------
# Dependency list
# ---------------

$(EXE_1) : $(OBJ_1)
	$(FL) $(EXTRA_FC_FLAGS) $(OBJ_1) $(FL_FLAGS) $(EXE_1)

$(EXE_2) : $(OBJ_2)
	$(FL) $(EXTRA_FC_FLAGS) $(OBJ_2) $(FL_FLAGS) $(EXE_2)


#---Dependence of the main program object
$(OBJ_MAIN_1) : $(SRC_1) ${OBJ_LIB} lsq.o
	$(FC) -c $(SRC_1) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_MAIN_1)

$(OBJ_MAIN_2) : $(SRC_2) ${OBJ_LIB}
	$(FC) -c $(SRC_2) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_MAIN_2)

lsq.o : lsq.f90
	$(FC) -c lsq.f90 

#${OBJ_LIB} : ${SRC_LIB}
#	cd $(lib_path)  ; make


# --------
# Clean up
# --------

clean:
	$(REMOVE) *.mod *.o $(EXE_S)
