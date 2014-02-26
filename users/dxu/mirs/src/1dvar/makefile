#==============================================================================
#
# Makefile for 1DVAR program
#
# S.A. Boukabara IMSG Inc. @ NOAA/NESDIS/ORA 2005-2006.
#==============================================================================

#---Paths are read from a common file (used across the MIRS package)
paths_path = ../../setup
include $(paths_path)/paths

#---Default macros and rules (from CRTM)
include $(CRTM_path)/make.macros
include $(CRTM_path)/make.rules

MAIN_path     = .

# -------------
# This makefile
# -------------
MAKEFILE = makefile

# ---------------
# Executable file
# ---------------
EXE_FILE = $(exec_path)/1dvar

# ------------
# Object files
# ------------
OBJ_FILE_MAIN  = $(MAIN_path)/1dvar.o

OBJ_FILES_SUB  = \
	$(lib_path)/misc/misc.o                 $(lib_path)/misc/CntrlParams.o           \
	$(lib_path)/misc/Consts.o               $(lib_path)/misc/TuningParams.o          \
	$(lib_path)/io/IO_Monitor.o             $(lib_path)/InversProcess/GeophCovBkg.o  \
	$(lib_path)/InversProcess/SeFeErrCov.o  $(lib_path)/io/IO_MeasurData.o           \
	$(lib_path)/qc/QCchecking.o             $(lib_path)/PrePostProcess/PreClassif.o  \
	$(lib_path)/noise/Noise.o               $(lib_path)/FwdOperProcess/FwdOperator.o \
	$(lib_path)/math/mathFcts.o             $(lib_path)/InversProcess/VarOprs.o      \
	$(lib_path)/io/IO_Scene.o                                                        \
	$(lib_path)/utilities/utils.o           $(lib_path)/io/IO_Noise.o                \
	$(lib_path)/qc/ErrorHandling.o	        $(lib_path)/io/IO_Colocate.o

OBJ_FILES      = $(OBJ_FILE_MAIN) $(OBJ_FILES_SUB)
SRC_FILES_SUB  = $(OBJ_FILES_SUB:.o=.f90)
SRC_FILES_CRTM = $(OBJ_FILES_CRTM:.o=.f90)
SRC_FILE_MAIN  = $(OBJ_FILE_MAIN:.o=.f90)


# -------------------------------
# Include and library definitions
# -------------------------------
INCLUDES  = \
	-I$(lib_path)/misc          -I$(lib_path)/io              -I$(lib_path)/math           \
	-I$(lib_path)/InversProcess -I$(lib_path)/FwdOperProcess  -I$(lib_path)/PrePostProcess \
	-I$(lib_path)/qc            -I$(lib_path)/noise           -I$(CRTM_path)/include       \
	-I$(lib_path)/utilities

LIBRARIES= \
	-L$(CRTM_path)/lib -lCRTM

# -----------------------
# Extra compilation flags
# -----------------------
EXTRA_FC_FLAGS = $(convEndOption)




#----------------
# Execution
#----------------
all:
	@case `uname -s` in \
		"SunOS"  ) make -f $(MAKEFILE) the_program $(SUNOS_FLAGS)  ;; \
		"AIX"    ) make -f $(MAKEFILE) the_program $(AIX_FLAGS)    ;; \
		"IRIX64" ) make -f $(MAKEFILE) the_program $(IRIX64_FLAGS) ;; \
		"Linux"  ) make -f $(MAKEFILE) the_program $(LINUX_FLAGS)  ;; \
		*        ) echo "This system is not supported"             ;; \
	esac

# ---------------------
# Make the test program
# ---------------------
the_program: $(EXE_FILE)




# ---------------
# Dependency list
# ---------------

$(EXE_FILE) : $(OBJ_FILE_MAIN) 
	$(FL) $(OBJ_FILES) $(FL_FLAGS) $(EXE_FILE) $(INCLUDES)

$(OBJ_FILE_MAIN) : $(SRC_FILE_MAIN) $(OBJ_FILES_SUB)
	$(FC) $(FC_FLAGS) $(EXTRA_FC_FLAGS) $(SRC_FILE_MAIN) -o $(OBJ_FILE_MAIN)

$(OBJ_FILES_SUB) : $(SRC_FILES_SUB)
	cd $(lib_path) ; make

# --------
# Clean up
# --------
clean:
	$(REMOVE) $(OBJ_FILES) $(EXE_FILE) \
	*.mod *.MOD *.stb *~ $(MAIN_path)/*~ ; cd $(lib_path) ; make clean
