#==============================================================================
#
# Makefile for TDR2SDR
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
EXE_FILE = $(exec_path)/tdr2sdr


# ------------
# Object files
# ------------
OBJ_MAIN_FILE     =  $(MAIN_path)/tdr2sdr.o
OBJ_CRTM_FILES    = 
OBJ_UTILITY_FILES =  $(lib_path)/io/IO_MeasurData.o $(lib_path)/io/IO_Noise.o $(lib_path)/misc/misc.o \
	             $(lib_path)/misc/Consts.o $(lib_path)/io/IO_Misc.o $(lib_path)/qc/ErrorHandling.o
OBJ_FILES         =  $(OBJ_CRTM_FILES) $(OBJ_UTILITY_FILES) $(OBJ_MAIN_FILE)

SRC_UTILITY_FILES = $(OBJ_UTILITY_FILES:.o=.f90)
SRC_CRTM_FILES    = $(OBJ_CRTM_FILES:.o=.f90)
SRC_MAIN_FILE     = $(OBJ_MAIN_FILE:.o=.f90)

# -------------------------------
# Include and library definitions
# -------------------------------
INCLUDES  =  -I$(CRTM_path)  -I$(lib_path)  -I$(lib_path)/misc  -I$(lib_path)/io  -I$(lib_path)/qc      

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
# Make the program
# ---------------------
the_program : $(EXE_FILE)





# ---------------
# Dependency list
# ---------------

$(EXE_FILE) : $(OBJ_MAIN_FILE)
	$(FL) $(OBJ_FILES) $(FL_FLAGS) $(EXE_FILE)

$(OBJ_MAIN_FILE) : $(SRC_MAIN_FILE) $(OBJ_UTILITY_FILES)
	$(FC) $(FC_FLAGS) $(EXTRA_FC_FLAGS) $(SRC_MAIN_FILE) -o $(OBJ_MAIN_FILE)

$(OBJ_UTILITY_FILES) : $(SRC_UTILITY_FILES)
	cd $(lib_path) ; make


# --------
# Clean up
# --------

clean:
	$(REMOVE) $(EXE_FILE) $(OBJ_FILES) $(lib_path)/*.mod $(lib_path)/*.MOD $(lib_path)/*.o \
	$(MAIN_path)/*.mod $(MAIN_path)/*.MOD $(MAIN_path)/*~ *.mod *.MOD *.stb ; \
	cd $(lib_path) ; make clean  ; cd $(CRTM_path) ; make clean
