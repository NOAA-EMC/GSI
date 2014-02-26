#==============================================================================
#
# Makefile for FM
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
EXE_FILE_1 = $(exec_path)/fm_f16
EXE_FILE_2 = $(exec_path)/modifyNedt_ssmis
EXE_FILE_3 = $(exec_path)/fm_n18
EXE_FILE_4 = $(exec_path)/fm_metopA
EXE_FILE_5 = $(exec_path)/fm_n19
EXE_FILE_6 = $(exec_path)/fm_f18
EXE_FILE_7 = $(exec_path)/fm_npp
EXE_FILE_8 = $(exec_path)/fm_aqua
EXE_FILE_9 = $(exec_path)/fm_fy3ri
EXE_FILE_a = $(exec_path)/fm_trmm
EXE_FILE_b = $(exec_path)/fm_mtsa
EXE_FILE_c = $(exec_path)/fm_mtma
EXE_FILE_d = $(exec_path)/fm_metopB
EXE_FILE_e = $(exec_path)/fm_gcomw1
EXE_FILE_f = $(exec_path)/fm_f17

EXE_FILE_S = $(EXE_FILE_1) $(EXE_FILE_2) $(EXE_FILE_3) $(EXE_FILE_4) $(EXE_FILE_5) \
             $(EXE_FILE_6) $(EXE_FILE_7) $(EXE_FILE_8) $(EXE_FILE_9) $(EXE_FILE_a) \
	     $(EXE_FILE_b) $(EXE_FILE_c) $(EXE_FILE_d) $(EXE_FILE_e) $(EXE_FILE_f)

# ------------
# Object files
# ------------
OBJ_MAIN_FILES_1  =  $(MAIN_path)/fm_f16.o
OBJ_MAIN_FILES_2  =  $(MAIN_path)/modifyNedt_ssmis.o
OBJ_MAIN_FILES_3  =  $(MAIN_path)/fm_n18.o
OBJ_MAIN_FILES_4  =  $(MAIN_path)/fm_metopA.o
OBJ_MAIN_FILES_5  =  $(MAIN_path)/fm_n19.o
OBJ_MAIN_FILES_6  =  $(MAIN_path)/fm_f18.o
OBJ_MAIN_FILES_7  =  $(MAIN_path)/fm_npp.o
OBJ_MAIN_FILES_8  =  $(MAIN_path)/fm_aqua.o
OBJ_MAIN_FILES_9  =  $(MAIN_path)/fm_fy3ri.o
OBJ_MAIN_FILES_a  =  $(MAIN_path)/fm_trmm.o
OBJ_MAIN_FILES_b  =  $(MAIN_path)/fm_mtsa.o
OBJ_MAIN_FILES_c  =  $(MAIN_path)/fm_mtma.o
OBJ_MAIN_FILES_d  =  $(MAIN_path)/fm_metopB.o
OBJ_MAIN_FILES_e  =  $(MAIN_path)/fm_gcomw1.o
OBJ_MAIN_FILES_f  =  $(MAIN_path)/fm_f17.o

OBJ_MAIN_FILES_S  = $(OBJ_MAIN_FILES_1) $(OBJ_MAIN_FILES_2) $(OBJ_MAIN_FILES_3) $(OBJ_MAIN_FILES_4) $(OBJ_MAIN_FILES_5) \
                    $(OBJ_MAIN_FILES_6) $(OBJ_MAIN_FILES_7) $(OBJ_MAIN_FILES_8) $(OBJ_MAIN_FILES_9) $(OBJ_MAIN_FILES_a) \
	            $(OBJ_MAIN_FILES_b) $(OBJ_MAIN_FILES_c) $(OBJ_MAIN_FILES_d) $(OBJ_MAIN_FILES_e) $(OBJ_MAIN_FILES_f)

# ------------
# Src files
# ------------
SRC_FILE_1  =  $(OBJ_MAIN_FILES_1:.o=.f90)
SRC_FILE_2  =  $(OBJ_MAIN_FILES_2:.o=.f90)
SRC_FILE_3  =  $(OBJ_MAIN_FILES_3:.o=.f90)
SRC_FILE_4  =  $(OBJ_MAIN_FILES_4:.o=.f90)
SRC_FILE_5  =  $(OBJ_MAIN_FILES_5:.o=.f90)
SRC_FILE_6  =  $(OBJ_MAIN_FILES_6:.o=.f90)
SRC_FILE_7  =  $(OBJ_MAIN_FILES_7:.o=.f90)
SRC_FILE_8  =  $(OBJ_MAIN_FILES_8:.o=.f90)
SRC_FILE_9  =  $(OBJ_MAIN_FILES_9:.o=.f90)
SRC_FILE_a  =  $(OBJ_MAIN_FILES_a:.o=.f90)
SRC_FILE_b  =  $(OBJ_MAIN_FILES_b:.o=.f90)
SRC_FILE_c  =  $(OBJ_MAIN_FILES_c:.o=.f90)
SRC_FILE_d  =  $(OBJ_MAIN_FILES_d:.o=.f90)
SRC_FILE_e  =  $(OBJ_MAIN_FILES_e:.o=.f90)
SRC_FILE_f  =  $(OBJ_MAIN_FILES_f:.o=.f90)


OBJ_CRTM_FILES    =
OBJ_UTILITY_FILES =  $(lib_path)/io/IO_MeasurData.o $(lib_path)/io/IO_Noise.o $(lib_path)/misc/misc.o \
		     $(lib_path)/misc/Consts.o $(lib_path)/io/IO_Misc.o $(lib_path)/qc/ErrorHandling.o \
		     $(lib_path)/noise/Noise.o $(lib_path)/math/mathFcts.o $(lib_path)/utilities/utils.o 
		     
OBJ_FILES_1       =  $(OBJ_CRTM_FILES) $(OBJ_UTILITY_FILES) $(OBJ_MAIN_FILES_1)
OBJ_FILES_2       =  $(OBJ_CRTM_FILES) $(OBJ_UTILITY_FILES) $(OBJ_MAIN_FILES_2)
OBJ_FILES_3       =  $(OBJ_CRTM_FILES) $(OBJ_UTILITY_FILES) $(OBJ_MAIN_FILES_3)
OBJ_FILES_4       =  $(OBJ_CRTM_FILES) $(OBJ_UTILITY_FILES) $(OBJ_MAIN_FILES_4)
OBJ_FILES_5       =  $(OBJ_CRTM_FILES) $(OBJ_UTILITY_FILES) $(OBJ_MAIN_FILES_5)
OBJ_FILES_6       =  $(OBJ_CRTM_FILES) $(OBJ_UTILITY_FILES) $(OBJ_MAIN_FILES_6)
OBJ_FILES_7       =  $(OBJ_CRTM_FILES) $(OBJ_UTILITY_FILES) $(OBJ_MAIN_FILES_7)
OBJ_FILES_8       =  $(OBJ_CRTM_FILES) $(OBJ_UTILITY_FILES) $(OBJ_MAIN_FILES_8)
OBJ_FILES_9       =  $(OBJ_CRTM_FILES) $(OBJ_UTILITY_FILES) $(OBJ_MAIN_FILES_9)
OBJ_FILES_a       =  $(OBJ_CRTM_FILES) $(OBJ_UTILITY_FILES) $(OBJ_MAIN_FILES_a)
OBJ_FILES_b       =  $(OBJ_CRTM_FILES) $(OBJ_UTILITY_FILES) $(OBJ_MAIN_FILES_b)
OBJ_FILES_c       =  $(OBJ_CRTM_FILES) $(OBJ_UTILITY_FILES) $(OBJ_MAIN_FILES_c)
OBJ_FILES_d       =  $(OBJ_CRTM_FILES) $(OBJ_UTILITY_FILES) $(OBJ_MAIN_FILES_d)
OBJ_FILES_e       =  $(OBJ_CRTM_FILES) $(OBJ_UTILITY_FILES) $(OBJ_MAIN_FILES_e)
OBJ_FILES_f       =  $(OBJ_CRTM_FILES) $(OBJ_UTILITY_FILES) $(OBJ_MAIN_FILES_f)

SRC_UTILITY_FILES = $(OBJ_UTILITY_FILES:.o=.f90) 
SRC_CRTM_FILES    = $(OBJ_CRTM_FILES:.o=.f90)

# -------------------------------
# Include and library definitions
# -------------------------------
INCLUDES = -I$(CRTM_path) -I$(lib_path)/io -I$(lib_path)/math \
           -I$(lib_path)/misc -I$(lib_path)/noise -I$(lib_path)/qc \
	   -I$(lib_path)/utilities

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
the_program : $(EXE_FILE_S)

# ---------------
# Dependency list
# ---------------
$(EXE_FILE_1) : $(OBJ_MAIN_FILES_1) 
	$(FL) $(OBJ_FILES_1) $(FL_FLAGS) $(EXE_FILE_1)
$(EXE_FILE_2) : $(OBJ_MAIN_FILES_2)
	$(FL) $(OBJ_FILES_2) $(FL_FLAGS) $(EXE_FILE_2)
$(EXE_FILE_3) : $(OBJ_MAIN_FILES_3)
	$(FL) $(OBJ_FILES_3) $(FL_FLAGS) $(EXE_FILE_3)
$(EXE_FILE_4) : $(OBJ_MAIN_FILES_4)
	$(FL) $(OBJ_FILES_4) $(FL_FLAGS) $(EXE_FILE_4)
$(EXE_FILE_5) : $(OBJ_MAIN_FILES_5)
	$(FL) $(OBJ_FILES_5) $(FL_FLAGS) $(EXE_FILE_5)
$(EXE_FILE_6) : $(OBJ_MAIN_FILES_6)
	$(FL) $(OBJ_FILES_6) $(FL_FLAGS) $(EXE_FILE_6)
$(EXE_FILE_7) : $(OBJ_MAIN_FILES_7)
	$(FL) $(OBJ_FILES_7) $(FL_FLAGS) $(EXE_FILE_7)
$(EXE_FILE_8) : $(OBJ_MAIN_FILES_8)
	$(FL) $(OBJ_FILES_8) $(FL_FLAGS) $(EXE_FILE_8)
$(EXE_FILE_9) : $(OBJ_MAIN_FILES_9)
	$(FL) $(OBJ_FILES_9) $(FL_FLAGS) $(EXE_FILE_9)
$(EXE_FILE_a) : $(OBJ_MAIN_FILES_a)
	$(FL) $(OBJ_FILES_a) $(FL_FLAGS) $(EXE_FILE_a)
$(EXE_FILE_b) : $(OBJ_MAIN_FILES_b)
	$(FL) $(OBJ_FILES_b) $(FL_FLAGS) $(EXE_FILE_b)
$(EXE_FILE_c) : $(OBJ_MAIN_FILES_c)
	$(FL) $(OBJ_FILES_c) $(FL_FLAGS) $(EXE_FILE_c)
$(EXE_FILE_d) : $(OBJ_MAIN_FILES_d)
	$(FL) $(OBJ_FILES_d) $(FL_FLAGS) $(EXE_FILE_d)
$(EXE_FILE_e) : $(OBJ_MAIN_FILES_e)
	$(FL) $(OBJ_FILES_e) $(FL_FLAGS) $(EXE_FILE_e)
$(EXE_FILE_f) : $(OBJ_MAIN_FILES_f)
	$(FL) $(OBJ_FILES_f) $(FL_FLAGS) $(EXE_FILE_f)


$(OBJ_MAIN_FILES_1) : $(SRC_FILE_1) $(OBJ_UTILITY_FILES) 
	$(FC) $(SRC_FILE_1) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_MAIN_FILES_1)
$(OBJ_MAIN_FILES_2) : $(SRC_FILE_2) $(OBJ_UTILITY_FILES) 
	$(FC) $(SRC_FILE_2) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_MAIN_FILES_2)
$(OBJ_MAIN_FILES_3) : $(SRC_FILE_3) $(OBJ_UTILITY_FILES) 
	$(FC) $(SRC_FILE_3) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_MAIN_FILES_3)
$(OBJ_MAIN_FILES_4) : $(SRC_FILE_4) $(OBJ_UTILITY_FILES) 
	$(FC) $(SRC_FILE_4) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_MAIN_FILES_4)
$(OBJ_MAIN_FILES_5) : $(SRC_FILE_5) $(OBJ_UTILITY_FILES) 
	$(FC) $(SRC_FILE_5) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_MAIN_FILES_5)
$(OBJ_MAIN_FILES_6) : $(SRC_FILE_6) $(OBJ_UTILITY_FILES) 
	$(FC) $(SRC_FILE_6) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_MAIN_FILES_6)
$(OBJ_MAIN_FILES_7) : $(SRC_FILE_7) $(OBJ_UTILITY_FILES)
	$(FC) $(SRC_FILE_7) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_MAIN_FILES_7)
$(OBJ_MAIN_FILES_8) : $(SRC_FILE_8) $(OBJ_UTILITY_FILES)
	$(FC) $(SRC_FILE_8) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_MAIN_FILES_8)
$(OBJ_MAIN_FILES_9) : $(SRC_FILE_9) $(OBJ_UTILITY_FILES) 
	$(FC) $(SRC_FILE_9) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_MAIN_FILES_9)
$(OBJ_MAIN_FILES_a) : $(SRC_FILE_a) $(OBJ_UTILITY_FILES) 
	$(FC) $(SRC_FILE_a) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_MAIN_FILES_a)
$(OBJ_MAIN_FILES_b) : $(SRC_FILE_b) $(OBJ_UTILITY_FILES) 
	$(FC) $(SRC_FILE_b) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_MAIN_FILES_b)
$(OBJ_MAIN_FILES_c) : $(SRC_FILE_c) $(OBJ_UTILITY_FILES) 
	$(FC) $(SRC_FILE_c) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_MAIN_FILES_c)
$(OBJ_MAIN_FILES_d) : $(SRC_FILE_d) $(OBJ_UTILITY_FILES) 
	$(FC) $(SRC_FILE_d) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_MAIN_FILES_d)
$(OBJ_MAIN_FILES_e) : $(SRC_FILE_e) $(OBJ_UTILITY_FILES) 
	$(FC) $(SRC_FILE_e) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_MAIN_FILES_e)
$(OBJ_MAIN_FILES_f) : $(SRC_FILE_f) $(OBJ_UTILITY_FILES) 
	$(FC) $(SRC_FILE_f) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_MAIN_FILES_f)

$(OBJ_UTILITY_FILES) : $(SRC_UTILITY_FILES)
	cd $(lib_path) ; make



# --------
# Clean up
# --------

clean:
	$(REMOVE) $(EXE_FILE_S) $(OBJ_MAIN_FILES_S) 
