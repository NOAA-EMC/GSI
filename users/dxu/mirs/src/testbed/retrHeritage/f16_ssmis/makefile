#========================================================================================
#
#    makefile for applying heritage algorithms
#
#========================================================================================

#-----------------------------------------------------------------------------
#                          -- Define macros --
#-----------------------------------------------------------------------------

#---Paths are read from a common file (used across the MIRS package)
paths_path = ../../../../setup
include $(paths_path)/paths


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
EXE_FILE = ${exec_path}/fmsdr2heriAlgoRetr


# ------------
# Object files
# ------------
OBJ_FILES_MAIN    =  ${MAIN_path}/fmsdr2heriAlgoRetr.o

OBJ_FILES_SUB  = \
  $(lib_path)/misc/misc.o                 $(lib_path)/misc/CntrlParams.o          \
  $(lib_path)/misc/Consts.o               $(lib_path)/misc/TuningParams.o         \
  $(lib_path)/io/IO_Monitor.o             $(lib_path)/InversProcess/GeophCovBkg.o \
  $(lib_path)/InversProcess/SeFeErrCov.o  $(lib_path)/io/IO_MeasurData.o          \
  $(lib_path)/qc/QCchecking.o             $(lib_path)/PrePostProcess/PreClassif.o \
  $(lib_path)/noise/Noise.o               $(lib_path)/utilities/utils.o           \
  $(lib_path)/math/mathFcts.o             $(lib_path)/InversProcess/VarOprs.o     \
  $(lib_path)/io/IO_Scene.o                                                       \
  $(lib_path)/io/IO_Noise.o               $(lib_path)/qc/ErrorHandling.o

OBJ_FILES_ALG  = \
  $(MAIN_path)/ssmis_heriAlgors.o

OBJ_FILES     = ${OBJ_FILES_MAIN} ${OBJ_FILES_SUB} ${OBJ_FILES_ALG}
SRC_FILES_SUB = ${OBJ_FILES_SUB:.o=.f90}
SRC_FILES_ALG = ${OBJ_FILES_ALG:.o=.f90}


# -------------------------------
# Include and library definitions
# -------------------------------
INCLUDES  = \
  -I$(lib_path)/misc          -I$(lib_path)/io              -I$(lib_path)/math           \
  -I$(lib_path)/InversProcess -I$(lib_path)/FwdOperProcess  -I$(lib_path)/PrePostProcess \
  -I$(lib_path)/qc            -I$(lib_path)/noise           -I$(lib_path)/utilities      \
  -I$(MAIN_path)


# -----------------------
# Extra compilation flags
# -----------------------
EXTRA_FC_FLAGS = $(INCLUDES) $(LIBRARIES)


#-----------------------------------------------------------------------------
#                          -- Define dependecies --
#-----------------------------------------------------------------------------

# ---------------------
# Make based on OS type
# ---------------------

all:
	@echo "OS type detected: " `uname -s` 
	@case `uname -s` in \
		"SunOS")   make -f $(MAKEFILE) the_program $(SUNOS_FLAGS)     ;; \
		"AIX")     make -f $(MAKEFILE) the_program $(AIX_FLAGS)       ;; \
		"IRIX64" ) make -f $(MAKEFILE) the_program $(IRIX64_FLAGS)    ;; \
		"Linux" )  make -f $(MAKEFILE) the_program $(LINUX_FLAGS) ;; \
		*) echo "This system is not supported" ;; \
	esac


# ---------------------
# Make the test program
# ---------------------
the_program : $(EXE_FILE)


# ---------------
# Dependency list
# ---------------

#---Dependence of the main program object
${MAIN_path}/fmsdr2heriAlgoRetr.o : ${MAIN_path}/fmsdr2heriAlgoRetr.f90 $(OBJ_FILES_SUB) $(OBJ_FILES_ALG)
	$(FC) $(FC_FLAGS)  ${MAIN_path}/fmsdr2heriAlgoRetr.f90 -o ${MAIN_path}/fmsdr2heriAlgoRetr.o

$(OBJ_FILES_SUB) : $(SRC_FILES_SUB)
	cd $(lib_path) ; make

$(OBJ_FILES_ALG) : $(SRC_FILES_ALG)
	cd $(MAIN_path) ; $(FC) $(EXTRA_FC_FLAGS) $(FC_FLAGS) ssmis_heriAlgors.f90


#---Dependence of the executable
$(EXE_FILE) : ${MAIN_path}/fmsdr2heriAlgoRetr.o 
	$(FL)  $(OBJ_FILES) $(FL_FLAGS)  $(EXE_FILE) $(INCLUDES)  


# --------
# Clean up
# --------
clean:
	$(REMOVE) \
	$(EXE_FILE) $(MAIN_path)/*.mod *.MOD *.o ;	cd $(lib_path) ; make clean


#---- End of makefile ----
