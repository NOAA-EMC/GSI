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
EXE_FILE_1 = $(exec_path)/gridEdr
EXE_FILE_2 = $(exec_path)/gridRad
EXE_FILE_3 = $(exec_path)/gridBias
EXE_FILE_4 = $(exec_path)/gridDep
EXE_FILE_5 = $(exec_path)/p2pEdr
EXE_FILE_6 = $(exec_path)/gridRain
EXE_FILE_7 = $(exec_path)/p2pDep
EXE_FILE_8 = $(exec_path)/gridDepClimate
EXE_FILE_9 = $(exec_path)/gridEdrClimate
EXE_FILE_a = $(exec_path)/gridVertical
EXE_FILE_b = $(exec_path)/dynamicBackground
EXE_FILE_c = $(exec_path)/p2pRad
EXE_FILE_d = $(exec_path)/interEdr
EXE_FILE_e = $(exec_path)/interDep
EXE_FILE_f = $(exec_path)/emspectrum
EXE_FILE_g = $(exec_path)/qcRetrieval

EXE_FILE = $(EXE_FILE_1) $(EXE_FILE_2) $(EXE_FILE_3) $(EXE_FILE_4) \
           $(EXE_FILE_5) $(EXE_FILE_6) $(EXE_FILE_7) $(EXE_FILE_8) \
	   $(EXE_FILE_9) $(EXE_FILE_a) $(EXE_FILE_b) $(EXE_FILE_c) \
	   $(EXE_FILE_d) $(EXE_FILE_e) $(EXE_FILE_f) $(EXE_FILE_g)


# ------------
# Object files
# ------------
OBJ_FILE_1 = $(MAIN_path)/gridEdr.o
OBJ_FILE_2 = $(MAIN_path)/gridRad.o
OBJ_FILE_3 = $(MAIN_path)/gridBias.o
OBJ_FILE_4 = $(MAIN_path)/gridDep.o
OBJ_FILE_5 = $(MAIN_path)/p2pEdr.o
OBJ_FILE_6 = $(MAIN_path)/gridRain.o
OBJ_FILE_7 = $(MAIN_path)/p2pDep.o
OBJ_FILE_8 = $(MAIN_path)/gridDepClimate.o
OBJ_FILE_9 = $(MAIN_path)/gridEdrClimate.o
OBJ_FILE_a = $(MAIN_path)/gridVertical.o
OBJ_FILE_b = $(MAIN_path)/dynamicBackground.o
OBJ_FILE_c = $(MAIN_path)/p2pRad.o
OBJ_FILE_d = $(MAIN_path)/interEdr.o
OBJ_FILE_e = $(MAIN_path)/interDep.o
OBJ_FILE_f = $(MAIN_path)/emspectrum.o
OBJ_FILE_g = $(MAIN_path)/qcRetrieval.o

OBJ_UTILITY_FILES  = \
	$(lib_path)/misc/Consts.o      \
	$(lib_path)/misc/misc.o        \
	$(lib_path)/utilities/utils.o  \
	$(lib_path)/noise/Noise.o      \
	$(lib_path)/io/IO_Noise.o      \
	$(lib_path)/io/IO_Scene.o      \
	$(lib_path)/io/IO_DEP.o	       \
	$(lib_path)/io/IO_MeasurData.o \
	$(lib_path)/io/IO_Misc.o       \
	$(lib_path)/qc/ErrorHandling.o \
	$(lib_path)/math/mathFcts.o

OBJ_FILES_1 = $(OBJ_UTILITY_FILES) $(OBJ_FILE_1)
OBJ_FILES_2 = $(OBJ_UTILITY_FILES) $(OBJ_FILE_2)
OBJ_FILES_3 = $(OBJ_UTILITY_FILES) $(OBJ_FILE_3)
OBJ_FILES_4 = $(OBJ_UTILITY_FILES) $(OBJ_FILE_4)
OBJ_FILES_5 = $(OBJ_UTILITY_FILES) $(OBJ_FILE_5)
OBJ_FILES_6 = $(OBJ_UTILITY_FILES) $(OBJ_FILE_6)
OBJ_FILES_7 = $(OBJ_UTILITY_FILES) $(OBJ_FILE_7)
OBJ_FILES_8 = $(OBJ_UTILITY_FILES) $(OBJ_FILE_8)
OBJ_FILES_9 = $(OBJ_UTILITY_FILES) $(OBJ_FILE_9)
OBJ_FILES_a = $(OBJ_UTILITY_FILES) $(OBJ_FILE_a)
OBJ_FILES_b = $(OBJ_UTILITY_FILES) $(OBJ_FILE_b)
OBJ_FILES_c = $(OBJ_UTILITY_FILES) $(OBJ_FILE_c)
OBJ_FILES_d = $(OBJ_UTILITY_FILES) $(OBJ_FILE_d)
OBJ_FILES_e = $(OBJ_UTILITY_FILES) $(OBJ_FILE_e)
OBJ_FILES_f = $(OBJ_UTILITY_FILES) $(OBJ_FILE_f)
OBJ_FILES_g = $(OBJ_UTILITY_FILES) $(OBJ_FILE_g)


# -----------------------
# Source files
# -----------------------
SRC_UTILITY_FILES = $(OBJ_UTILITY_FILES:.o=.f90)

SRC_FILE_1 = $(OBJ_FILE_1:.o=.f90)
SRC_FILE_2 = $(OBJ_FILE_2:.o=.f90)
SRC_FILE_3 = $(OBJ_FILE_3:.o=.f90)
SRC_FILE_4 = $(OBJ_FILE_4:.o=.f90)
SRC_FILE_5 = $(OBJ_FILE_5:.o=.f90)
SRC_FILE_6 = $(OBJ_FILE_6:.o=.f90)
SRC_FILE_7 = $(OBJ_FILE_7:.o=.f90)
SRC_FILE_8 = $(OBJ_FILE_8:.o=.f90)
SRC_FILE_9 = $(OBJ_FILE_9:.o=.f90)
SRC_FILE_a = $(OBJ_FILE_a:.o=.f90)
SRC_FILE_b = $(OBJ_FILE_b:.o=.f90)
SRC_FILE_c = $(OBJ_FILE_c:.o=.f90)
SRC_FILE_d = $(OBJ_FILE_d:.o=.f90)
SRC_FILE_e = $(OBJ_FILE_e:.o=.f90)
SRC_FILE_f = $(OBJ_FILE_f:.o=.f90)
SRC_FILE_g = $(OBJ_FILE_g:.o=.f90)


# -------------------------------
# Include and library definitions
# -------------------------------
INCLUDES  = -I$(lib_path)           \
	    -I$(lib_path)/misc      \
	    -I$(lib_path)/utilities \
	    -I$(lib_path)/noise     \
	    -I$(lib_path)/io        \
	    -I$(lib_path)/qc        \
            -I$(lib_path)/math

# -----------------------
# Extra compilation flags
# -----------------------
EXTRA_FC_FLAGS = $(convEndOption)

# ---------------------
# Make based on OS type
# ---------------------

all:
	@echo "OS type detected: "`uname -s` 
	@case `uname -s` in \
		"SunOS" )  make -f $(MAKEFILE) the_program $(SUNOS_FLAGS)     ;; \
		"AIX" )    make -f $(MAKEFILE) the_program $(AIX_FLAGS)       ;; \
		"IRIX64" ) make -f $(MAKEFILE) the_program $(IRIX64_FLAGS)    ;; \
		"Linux" )  make -f $(MAKEFILE) the_program $(LINUX_FLAGS)     ;; \
		*) echo "This system is not supported" ;; \
	esac


# ---------------------
# Make program
# ---------------------
the_program : $(EXE_FILE)


# ---------------
# Dependency list
# ---------------

$(EXE_FILE_1) : $(OBJ_FILES_1)
	$(FL) $(OBJ_FILES_1) $(FL_FLAGS) $(EXE_FILE_1)

$(EXE_FILE_2) : $(OBJ_FILES_2)
	$(FL) $(OBJ_FILES_2) $(FL_FLAGS) $(EXE_FILE_2)

$(EXE_FILE_3) : $(OBJ_FILES_3)
	$(FL) $(OBJ_FILES_3) $(FL_FLAGS) $(EXE_FILE_3)

$(EXE_FILE_4) : $(OBJ_FILES_4)
	$(FL) $(OBJ_FILES_4) $(FL_FLAGS) $(EXE_FILE_4)

$(EXE_FILE_5) : $(OBJ_FILES_5)
	$(FL) $(OBJ_FILES_5) $(FL_FLAGS) $(EXE_FILE_5)

$(EXE_FILE_6) : $(OBJ_FILES_6)
	$(FL) $(OBJ_FILES_6) $(FL_FLAGS) $(EXE_FILE_6)

$(EXE_FILE_7) : $(OBJ_FILES_7)
	$(FL) $(OBJ_FILES_7) $(FL_FLAGS) $(EXE_FILE_7)

$(EXE_FILE_8) : $(OBJ_FILES_8)
	$(FL) $(OBJ_FILES_8) $(FL_FLAGS) $(EXE_FILE_8)

$(EXE_FILE_9) : $(OBJ_FILES_9)
	$(FL) $(OBJ_FILES_9) $(FL_FLAGS) $(EXE_FILE_9)

$(EXE_FILE_a) : $(OBJ_FILES_a)
	$(FL) $(OBJ_FILES_a) $(FL_FLAGS) $(EXE_FILE_a)

$(EXE_FILE_b) : $(OBJ_FILES_b)
	$(FL) $(OBJ_FILES_b) $(FL_FLAGS) $(EXE_FILE_b)

$(EXE_FILE_c) : $(OBJ_FILES_c)
	$(FL) $(OBJ_FILES_c) $(FL_FLAGS) $(EXE_FILE_c)

$(EXE_FILE_d) : $(OBJ_FILES_d)
	$(FL) $(OBJ_FILES_d) $(FL_FLAGS) $(EXE_FILE_d)

$(EXE_FILE_e) : $(OBJ_FILES_e)
	$(FL) $(OBJ_FILES_e) $(FL_FLAGS) $(EXE_FILE_e)

$(EXE_FILE_f) : $(OBJ_FILES_f)
	$(FL) $(OBJ_FILES_f) $(FL_FLAGS) $(EXE_FILE_f)

$(EXE_FILE_g) : $(OBJ_FILES_g)
	$(FL) $(OBJ_FILES_g) $(FL_FLAGS) $(EXE_FILE_g)




$(OBJ_FILE_1) : $(SRC_FILE_1) $(OBJ_UTILITY_FILES)
	$(FC) $(SRC_FILE_1) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_FILE_1)

$(OBJ_FILE_2) : $(SRC_FILE_2) $(OBJ_UTILITY_FILES)
	$(FC) $(SRC_FILE_2) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_FILE_2)

$(OBJ_FILE_3) : $(SRC_FILE_3) $(OBJ_UTILITY_FILES)
	$(FC) $(SRC_FILE_3) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_FILE_3)

$(OBJ_FILE_4) : $(SRC_FILE_4) $(OBJ_UTILITY_FILES)
	$(FC) $(SRC_FILE_4) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_FILE_4)

$(OBJ_FILE_5) : $(SRC_FILE_5) $(OBJ_UTILITY_FILES)
	$(FC) $(SRC_FILE_5) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_FILE_5)

$(OBJ_FILE_6) : $(SRC_FILE_6) $(OBJ_UTILITY_FILES)
	$(FC) $(SRC_FILE_6) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_FILE_6)

$(OBJ_FILE_7) : $(SRC_FILE_7) $(OBJ_UTILITY_FILES)
	$(FC) $(SRC_FILE_7) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_FILE_7)

$(OBJ_FILE_8) : $(SRC_FILE_8) $(OBJ_UTILITY_FILES)
	$(FC) $(SRC_FILE_8) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_FILE_8)

$(OBJ_FILE_9) : $(SRC_FILE_9) $(OBJ_UTILITY_FILES)
	$(FC) $(SRC_FILE_9) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_FILE_9)

$(OBJ_FILE_a) : $(SRC_FILE_a) $(OBJ_UTILITY_FILES)
	$(FC) $(SRC_FILE_a) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_FILE_a)

$(OBJ_FILE_b) : $(SRC_FILE_b) $(OBJ_UTILITY_FILES)
	$(FC) $(SRC_FILE_b) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_FILE_b)

$(OBJ_FILE_c) : $(SRC_FILE_c) $(OBJ_UTILITY_FILES)
	$(FC) $(SRC_FILE_c) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_FILE_c)

$(OBJ_FILE_d) : $(SRC_FILE_d) $(OBJ_UTILITY_FILES)
	$(FC) $(SRC_FILE_d) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_FILE_d)

$(OBJ_FILE_e) : $(SRC_FILE_e) $(OBJ_UTILITY_FILES)
	$(FC) $(SRC_FILE_e) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_FILE_e)

$(OBJ_FILE_f) : $(SRC_FILE_f) $(OBJ_UTILITY_FILES)
	$(FC) $(SRC_FILE_f) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_FILE_f)

$(OBJ_FILE_g) : $(SRC_FILE_g) $(OBJ_UTILITY_FILES)
	$(FC) $(SRC_FILE_g) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_FILE_g)


$(OBJ_UTILITY_FILES) : $(SRC_UTILITY_FILES)
	cd $(lib_path) ; make


# ---------------
# clean up
# ---------------

clean:
	$(REMOVE) $(EXE_FILE) $(OBJ_FILE_1) $(OBJ_FILE_2) $(OBJ_FILE_3) $(OBJ_FILE_4) \
	$(OBJ_FILE_5) $(OBJ_FILE_6) $(OBJ_FILE_7) $(OBJ_FILE_8) $(OBJ_FILE_9) $(OBJ_FILE_a) \
        $(OBJ_FILE_b) $(OBJ_FILE_c) $(OBJ_FILE_d) $(OBJ_FILE_e) $(OBJ_FILE_f) $(OBJ_FILE_g) 

