#==============================================================================
#
# Makefile for Forward model
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
EXE_FILE = $(exec_path)/mergeNEDT
EXE_FILE2 = $(exec_path)/mergeNedt_npp_atms


# ------------
# Object files
# ------------
OBJ_MAIN_FILE =  $(MAIN_path)/mergeNEDT.o
OBJ_MAIN_FILE2 =  $(MAIN_path)/mergeNedt_npp_atms.o


OBJ_UTILITY_FILES =  $(lib_path)/io/IO_Noise.o $(lib_path)/noise/Noise.o $(lib_path)/misc/misc.o \
		     $(lib_path)/misc/Consts.o $(lib_path)/qc/ErrorHandling.o

OBJ_FILES         =  $(OBJ_UTILITY_FILES) $(OBJ_MAIN_FILE)
OBJ_FILES2        =  $(OBJ_MAIN_FILE2)

SRC_UTILITY_FILES = $(OBJ_UTILITY_FILES:.o=.f90)
SRC_MAIN_FILE     = $(OBJ_MAIN_FILE:.o=.f90) 
SRC_MAIN_FILE2     = $(OBJ_MAIN_FILE2:.o=.f90) 

# -------------------------------
# Include and library definitions
# -------------------------------
INCLUDES  = \
	-I$(lib_path)/io    -I$(lib_path)/misc  -I$(lib_path)/utilities \
	-I$(CRTM_path) -I$(lib_path)/noise -I$(lib_path)/qc

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
the_program : $(EXE_FILE) $(EXE_FILE2)





# ---------------
# Dependency list
# ---------------
$(EXE_FILE) : $(OBJ_MAIN_FILE)
	$(FL) $(OBJ_FILES) $(FL_FLAGS) $(EXE_FILE)

$(OBJ_MAIN_FILE) : $(SRC_MAIN_FILE) $(OBJ_UTILITY_FILES) 
	$(FC) $(EXTRA_FC_FLAGS) $(FC_FLAGS) $(SRC_MAIN_FILE) -o $(OBJ_MAIN_FILE)

$(OBJ_UTILITY_FILES) : $(SRC_UTILITY_FILES)
	cd $(lib_path)  ; make

$(EXE_FILE2) : $(OBJ_MAIN_FILE2)
	$(FL) $(OBJ_FILES2) $(FL_FLAGS) $(EXE_FILE2)

$(OBJ_MAIN_FILE2) : $(SRC_MAIN_FILE2)
	$(FC) $(EXTRA_FC_FLAGS) $(FC_FLAGS) $(SRC_MAIN_FILE2) -o $(OBJ_MAIN_FILE2)



# --------
# Clean up
# --------

clean:
	$(REMOVE) $(EXE_FILE) $(OBJ_FILES) $(EXE_FILE2) $(OBJ_FILES2) $(MAIN_PATH)/*.mod $(MAIN_PATH)/*.MOD \
	*.mod *.MOD *.stb ; cd $(lib_path) ; make clean 
