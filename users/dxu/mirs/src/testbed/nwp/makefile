#==============================================================================
#
# Makefile for program that colocates NWP data with radiances
#
#==============================================================================

#-----------------------------------------------------------------------------
#                          -- Define macros --
#-----------------------------------------------------------------------------

#---Paths are read from a common file (used across the MIRS package)
paths_path = ../../../setup
include $(paths_path)/paths

MAIN_path     = .
CURR_path     = `pwd`
#---Default macros and rules (from CRTM)
include $(CRTM_path)/make.macros
include $(CRTM_path)/make.rules


# -------------
# This makefile
# -------------
MAKEFILE = makefile

# ----------------
# Executable 
# ----------------
EXE_FILE1 = $(exec_path)/colocNWPwRad
EXE_FILE2 = $(exec_path)/prepNWP

EXE_FILE = $(EXE_FILE1) $(EXE_FILE2)


# ------------
# Object files
# ------------
OBJ_MAIN_FILES1 = $(MAIN_path)/colocNWPwRad.o
OBJ_MAIN_FILES2 = $(MAIN_path)/prepNWP.o

SRC_MAIN_FILES1 = $(MAIN_path)/colocNWPwRad.f90
SRC_MAIN_FILES2 = $(MAIN_path)/prepNWP.f90


OBJ_UTILITY_FILES  = \
	$(lib_path)/misc/misc.o                 $(lib_path)/misc/CntrlParams.o          \
	$(lib_path)/misc/Consts.o               $(lib_path)/misc/TuningParams.o         \
	$(lib_path)/io/IO_Monitor.o             $(lib_path)/InversProcess/GeophCovBkg.o \
	$(lib_path)/InversProcess/SeFeErrCov.o  $(lib_path)/io/IO_MeasurData.o          \
	$(lib_path)/qc/QCchecking.o             $(lib_path)/PrePostProcess/PreClassif.o \
	$(lib_path)/noise/Noise.o               $(lib_path)/FwdOperProcess/FwdOperator.o\
	$(lib_path)/math/mathFcts.o             $(lib_path)/InversProcess/VarOprs.o     \
	$(lib_path)/io/IO_Scene.o               $(lib_path)/io/IO_Misc_LE.o             \
	$(lib_path)/utilities/utils.o           $(lib_path)/io/IO_Noise.o               \
	$(lib_path)/io/IO_Misc.o                $(lib_path)/qc/ErrorHandling.o




OBJ_FILES1 = $(OBJ_UTILITY_FILES) $(OBJ_MAIN_FILES1)
OBJ_FILES2 = $(OBJ_UTILITY_FILES) $(OBJ_MAIN_FILES2)



SRC_UTILITY_FILES = $(OBJ_UTILITY_FILES:.o=.f90)

# -------------------------------
# Include and library definitions
# -------------------------------
INCLUDES  = -I$(CRTM_path)/include      -I$(lib_path) \
	    -I$(lib_path)/misc          -I$(lib_path)/io              -I$(lib_path)/math           \
	    -I$(lib_path)/InversProcess -I$(lib_path)/FwdOperProcess  -I$(lib_path)/PrePostProcess \
	    -I$(lib_path)/qc            -I$(lib_path)/noise           -I$(CRTM_path)               \
	    -I$(lib_path)/utilities

LIBRARIES = \
	-L$(CRTM_path)/lib -lCRTM

# -----------------------
# Extra compilation flags
# -----------------------
EXTRA_FC_FLAGS = $(convEndOption)


#-----------------------------------------------------------------------------
#                          -- Define dependecies --
#-----------------------------------------------------------------------------


# ---------------------
# Make based on OS type
# ---------------------

all:
	@echo "OS type detected: "`uname -s` 
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
the_program : $(EXE_FILE)


# ---------------
# Dependency list
# ---------------

#---Dependence of the main program object
$(OBJ_MAIN_FILES1) : $(SRC_MAIN_FILES1) $(OBJ_UTILITY_FILES)
	$(FC) -c $(SRC_MAIN_FILES1) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_MAIN_FILES1)

$(OBJ_MAIN_FILES2) : $(SRC_MAIN_FILES2) $(OBJ_UTILITY_FILES)
	$(FC) -c $(SRC_MAIN_FILES2) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_MAIN_FILES2)

$(OBJ_UTILITY_FILES) : $(SRC_UTILITY_FILES)
	cd $(lib_path)  ; make

#---Dependence of the executable
$(EXE_FILE1) : $(OBJ_MAIN_FILES1)
	$(FL) $(EXTRA_FC_FLAGS) $(OBJ_FILES1) $(FL_FLAGS) $(EXE_FILE1)

$(EXE_FILE2) : $(OBJ_MAIN_FILES2)
	$(FL) $(EXTRA_FC_FLAGS) $(OBJ_FILES2) $(FL_FLAGS) $(EXE_FILE2)




# --------
# Clean up
# --------

clean:
	$(REMOVE) $(EXE_FILE) $(OBJ_MAIN_FILES1) $(OBJ_MAIN_FILES2)
