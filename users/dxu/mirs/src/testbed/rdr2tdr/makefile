SHELL = /bin/sh

#---Paths are read from a common file (used across the MIRS package)
paths_path = ../../../setup
include $(paths_path)/paths
lib_path_cpp = ${MIRS_ROOT}/src/lib_cpp

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
EXE_1 = $(exec_path)/rdr2tdr_n18_amsua
EXE_2 = $(exec_path)/rdr2tdr_n18_mhs
EXE_3 = $(exec_path)/rdr2tdr_metopA_amsua
EXE_4 = $(exec_path)/rdr2tdr_metopA_mhs
EXE_5 = $(exec_path)/calib_f16_ssmis
EXE_6 = $(exec_path)/rdr2tdr_f16_ssmis
EXE_7 = $(exec_path)/rdr2tdr_n19_amsua
EXE_8 = $(exec_path)/rdr2tdr_n19_mhs
EXE_a = $(exec_path)/rdr2tdr_f18_ssmis
EXE_b = $(exec_path)/rdr2tdr_npp_atms
EXE_c = $(exec_path)/rdr2tdr_npp_atms_proxy
EXE_d = $(exec_path)/rdr2tdr_fy3ri_mwri
EXE_e = $(exec_path)/rdr2tdr_trmm_tmi
EXE_f = $(exec_path)/rdr2tdr_mtma_madras
EXE_g = $(exec_path)/rdr2tdr_mtsa_saphir
EXE_h = $(exec_path)/rdr2tdr_metopB_amsua
EXE_i = $(exec_path)/rdr2tdr_metopB_mhs
EXE_j = $(exec_path)/rdr2tdr_gcomw1_amsr2
EXE_k = $(exec_path)/rdr2tdr_f17_ssmis

EXE_S = $(EXE_1) $(EXE_2) $(EXE_3) $(EXE_4) $(EXE_5) $(EXE_6) $(EXE_7) $(EXE_8) \
        $(EXE_a) $(EXE_b) $(EXE_e) $(EXE_f) $(EXE_g) $(EXE_h) $(EXE_i) $(EXE_j) \
        $(EXE_k)

# ------------
# Main Object
# ------------
OBJ_MAIN_1  = $(MAIN_path)/rdr2tdr_n18_amsua.o
OBJ_MAIN_2  = $(MAIN_path)/rdr2tdr_n18_mhs.o
OBJ_MAIN_3  = $(MAIN_path)/rdr2tdr_metopA_amsua.o
OBJ_MAIN_4  = $(MAIN_path)/rdr2tdr_metopA_mhs.o
OBJ_MAIN_5  = $(MAIN_path)/calib_f16_ssmis.o
OBJ_MAIN_6  = $(MAIN_path)/rdr2tdr_f16_ssmis.o
OBJ_MAIN_7  = $(MAIN_path)/rdr2tdr_n19_amsua.o
OBJ_MAIN_8  = $(MAIN_path)/rdr2tdr_n19_mhs.o
OBJ_MAIN_a  = $(MAIN_path)/rdr2tdr_f18_ssmis.o
OBJ_MAIN_b  = $(MAIN_path)/rdr2tdr_npp_atms.o
OBJ_MAIN_c  = $(MAIN_path)/rdr2tdr_npp_atms_proxy.o
OBJ_MAIN_d  = $(MAIN_path)/rdr2tdr_fy3ri_mwri.o
OBJ_MAIN_e  = $(MAIN_path)/rdr2tdr_trmm_tmi.o
OBJ_MAIN_f  = $(MAIN_path)/rdr2tdr_mtma_madras.o
OBJ_MAIN_g  = $(MAIN_path)/rdr2tdr_mtsa_saphir.o
OBJ_MAIN_h  = $(MAIN_path)/rdr2tdr_metopB_amsua.o
OBJ_MAIN_i  = $(MAIN_path)/rdr2tdr_metopB_mhs.o
OBJ_MAIN_j  = $(MAIN_path)/rdr2tdr_gcomw1_amsr2.o
OBJ_MAIN_k  = $(MAIN_path)/rdr2tdr_f17_ssmis.o

OBJ_MAIN_S = $(OBJ_MAIN_1)  $(OBJ_MAIN_2)  $(OBJ_MAIN_3) \
             $(OBJ_MAIN_4)  $(OBJ_MAIN_5)  $(OBJ_MAIN_6) \
             $(OBJ_MAIN_7)  $(OBJ_MAIN_8)  $(OBJ_MAIN_a) \
             $(OBJ_MAIN_b)  $(OBJ_MAIN_c)  $(OBJ_MAIN_d) \
             $(OBJ_MAIN_e)  $(OBJ_MAIN_f)  $(OBJ_MAIN_g) \
	     $(OBJ_MAIN_h)  $(OBJ_MAIN_i)  $(OBJ_MAIN_j) \
	     $(OBJ_MAIN_k)

# ------------
# Main Src
# ------------
SRC_MAIN_1  = $(MAIN_path)/rdr2tdr_n18_amsua.f90
SRC_MAIN_2  = $(MAIN_path)/rdr2tdr_n18_mhs.f90
SRC_MAIN_3  = $(MAIN_path)/rdr2tdr_metopA_amsua.f90
SRC_MAIN_4  = $(MAIN_path)/rdr2tdr_metopA_mhs.f90
SRC_MAIN_5  = $(MAIN_path)/calib_f16_ssmis.f90
SRC_MAIN_6  = $(MAIN_path)/rdr2tdr_f16_ssmis.f90
SRC_MAIN_7  = $(MAIN_path)/rdr2tdr_n19_amsua.f90
SRC_MAIN_8  = $(MAIN_path)/rdr2tdr_n19_mhs.f90
SRC_MAIN_a  = $(MAIN_path)/rdr2tdr_f18_ssmis.f90
SRC_MAIN_b  = $(MAIN_path)/rdr2tdr_npp_atms.cc
SRC_MAIN_c  = $(MAIN_path)/rdr2tdr_npp_atms_proxy.cc
SRC_MAIN_d  = $(MAIN_path)/rdr2tdr_fy3ri_mwri.f90
SRC_MAIN_e  = $(MAIN_path)/rdr2tdr_trmm_tmi.f90
SRC_MAIN_f  = $(MAIN_path)/rdr2tdr_mtma_madras.cc
SRC_MAIN_g  = $(MAIN_path)/rdr2tdr_mtsa_saphir.cc
SRC_MAIN_h  = $(MAIN_path)/rdr2tdr_metopB_amsua.f90
SRC_MAIN_i  = $(MAIN_path)/rdr2tdr_metopB_mhs.f90
SRC_MAIN_j  = $(MAIN_path)/rdr2tdr_gcomw1_amsr2.cc
SRC_MAIN_k  = $(MAIN_path)/rdr2tdr_f17_ssmis.f90


#----Comment out SSMIS related programs as they are not standard compliant
OBJ_UTILITY = $(lib_path)/io/IO_MeasurData.o \
              $(lib_path)/io/IO_Noise.o \
              $(lib_path)/io/IO_InstrConfig.o \
              $(lib_path)/misc/misc.o \
              $(lib_path)/misc/Consts.o \
              $(lib_path)/io/IO_Misc.o \
              $(lib_path)/qc/ErrorHandling.o \
              $(lib_path)/io/io_ssmis.o \
              $(MAIN_path)/calib_ssmis_sub.o

OBJ_MT = $(lib_path_cpp)/mt.o
		    
OBJ_1 = $(OBJ_MAIN_1) $(OBJ_UTILITY)
OBJ_2 = $(OBJ_MAIN_2) $(OBJ_UTILITY)
OBJ_3 = $(OBJ_MAIN_3) $(OBJ_UTILITY)
OBJ_4 = $(OBJ_MAIN_4) $(OBJ_UTILITY)
OBJ_5 = $(OBJ_MAIN_5) $(OBJ_UTILITY)
OBJ_6 = $(OBJ_MAIN_6) $(OBJ_UTILITY)
OBJ_7 = $(OBJ_MAIN_7) $(OBJ_UTILITY)
OBJ_8 = $(OBJ_MAIN_8) $(OBJ_UTILITY)
OBJ_a = $(OBJ_MAIN_a) $(OBJ_UTILITY)
OBJ_b = $(OBJ_MAIN_b)
OBJ_c = $(OBJ_MAIN_c)
OBJ_d = $(OBJ_MAIN_d) $(OBJ_UTILITY)
OBJ_e = $(OBJ_MAIN_e) $(OBJ_UTILITY)
OBJ_f = $(OBJ_MAIN_f) $(OBJ_MT)
OBJ_g = $(OBJ_MAIN_g) $(OBJ_MT)
OBJ_h = $(OBJ_MAIN_h) $(OBJ_UTILITY)
OBJ_i = $(OBJ_MAIN_i) $(OBJ_UTILITY)
OBJ_j = $(OBJ_MAIN_j)
OBJ_k = $(OBJ_MAIN_k) $(OBJ_UTILITY)


SRC_UTILITY = $(OBJ_UTILITY:.o=.f90)
SRC_MT      = $(OBJ_MT:.o=.cc)
SRC_MT_HEAD = $(OBJ_MT:.o=.h)

# -------------------------------
# Include and library definitions
# -------------------------------
INCLUDES = -I$(CRTM_path)   -I$(lib_path)     -I$(lib_path)/misc \
           -I$(lib_path)/io -I$(lib_path)/qc  -I$(MAIN_path)

# -----------------------
# Extra compilation flags
# -----------------------
EXTRA_FC_FLAGS = $(convEndOption)


# -----------------------
# Extra flags for NPP ATMS
# -----------------------
INCLUDE_NPP = -I$(HDF5INC) -I$(SZIPINC) -I$(ZLIBINC)
LIBS_NPP    = -L$(HDF5LIB) -L$(SZIPLIB) -L$(ZLIBLIB)
LDFLAGS_NPP = -lhdf5_cpp -lhdf5 -lsz -lz -lm

# -----------------------
# Extra flags for FY3
# -----------------------
INCLUDE_FY3 = -I$(HDF4INC) -I$(SZIPINC) $(INCLUDES)
LIBS_FY3 = -L$(HDF4LIB) -L$(SZIPLIB)
LDFLAGS_FY3 = -lmfhdf -ldf -ljpeg -lsz -lz -lm 
FL_FLAGS_FY3 = $(LIBS_FY3) $(LDFLAGS_FY3) $(FL_FLAGS)

# -----------------------
# Extra flags for TRMM TMI
# -----------------------
INCLUDE_TRMM  = -I$(HDFEOSINC) -I$(HDF4INC) -I$(SZIPINC) $(INCLUDES)
LIBS_TRMM     = -L$(HDFEOSLIB) -L$(HDF4LIB) -L$(SZIPLIB)
LDFLAGS_TRMM  = -lhdfeos -lGctp -lmfhdf -ldf -ljpeg -lz -lm -lsz 
FL_FLAGS_TRMM = $(LIBS_TRMM) $(LDFLAGS_TRMM) $(FL_FLAGS)


# -----------------------
# Extra flags for MT MADRAS and SAPHIR
# -----------------------
INCLUDE_MT  = -I$(HDF5INC) -I$(ZLIBINC) -I$(lib_path_cpp)
LIBS_MT     = -L$(HDF5LIB) -L$(ZLIBLIB)
LDFLAGS_MT  = -lhdf5_cpp -lhdf5 -lz -lm


# -----------------------
# Extra flags for GCOM AMSR2
# -----------------------
INCLUDE_GCOM = -I$(HDF5INC) -I$(SZIPINC) -I$(ZLIBINC)
LIBS_GCOM    = -L$(HDF5LIB) -L$(SZIPLIB) -L$(ZLIBLIB)
LDFLAGS_GCOM = -lhdf5_cpp -lhdf5 -lsz -lz -lm



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
the_program : $(EXE_S)



# ---------------
# Dependency list
# ---------------

#---Dependence of the main program object
$(OBJ_MAIN_1) : $(SRC_MAIN_1) $(OBJ_UTILITY)
	$(FC) $(SRC_MAIN_1) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_MAIN_1)
$(OBJ_MAIN_2) : $(SRC_MAIN_2) $(OBJ_UTILITY)
	$(FC) $(SRC_MAIN_2) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_MAIN_2)
$(OBJ_MAIN_3) : $(SRC_MAIN_3) $(OBJ_UTILITY)
	$(FC) $(SRC_MAIN_3) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_MAIN_3)
$(OBJ_MAIN_4) : $(SRC_MAIN_4) $(OBJ_UTILITY)
	$(FC) $(SRC_MAIN_4) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_MAIN_4)
$(OBJ_MAIN_5) : $(SRC_MAIN_5) $(OBJ_UTILITY)
	$(FC) $(SRC_MAIN_5) $(EXTRA_FC_FLAGS) $(FC_FLAGS) $(qsaveOption) -o $(OBJ_MAIN_5)
$(OBJ_MAIN_6) : $(SRC_MAIN_6) $(OBJ_UTILITY)
	$(FC) $(SRC_MAIN_6) $(EXTRA_FC_FLAGS) $(FC_FLAGS) $(qsaveOption) -o $(OBJ_MAIN_6)
$(OBJ_MAIN_7) : $(SRC_MAIN_7) $(OBJ_UTILITY)
	$(FC) $(SRC_MAIN_7) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_MAIN_7)
$(OBJ_MAIN_8) : $(SRC_MAIN_8) $(OBJ_UTILITY)
	$(FC) $(SRC_MAIN_8) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_MAIN_8)
$(OBJ_MAIN_a) : $(SRC_MAIN_a) $(OBJ_UTILITY)
	$(FC) $(SRC_MAIN_a) $(EXTRA_FC_FLAGS) $(FC_FLAGS) $(qsaveOption) -o $(OBJ_MAIN_a)
$(OBJ_MAIN_b) : $(SRC_MAIN_b)
	$(CXX) $(CXX_FLAGS) $(INCLUDE_NPP) -c $(SRC_MAIN_b) -o $(OBJ_MAIN_b)
$(OBJ_MAIN_c) : $(SRC_MAIN_c) $(lib_path_cpp)/MeasurementType.h
	$(CXX) $(CXX_FLAGS) $(INCLUDE_NPP) -c $(SRC_MAIN_c) -o $(OBJ_MAIN_c)
$(OBJ_MAIN_d) : $(SRC_MAIN_d) $(OBJ_UTILITY)
	$(FC) $(INCLUDE_FY3) $(SRC_MAIN_d) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_MAIN_d)
$(OBJ_MAIN_e) : $(SRC_MAIN_e) $(OBJ_UTILITY)
	$(FC) $(INCLUDE_TRMM) $(SRC_MAIN_e) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_MAIN_e)
$(OBJ_MAIN_f) : $(SRC_MAIN_f)
	$(CXX) $(CXX_FLAGS) $(INCLUDE_MT) -c $(SRC_MAIN_f) -o $(OBJ_MAIN_f)
$(OBJ_MAIN_g) : $(SRC_MAIN_g)
	$(CXX) $(CXX_FLAGS) $(INCLUDE_MT) -c $(SRC_MAIN_g) -o $(OBJ_MAIN_g)
$(OBJ_MAIN_h) : $(SRC_MAIN_h) $(OBJ_UTILITY)
	$(FC) $(SRC_MAIN_h) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_MAIN_h)
$(OBJ_MAIN_i) : $(SRC_MAIN_i) $(OBJ_UTILITY)
	$(FC) $(SRC_MAIN_i) $(EXTRA_FC_FLAGS) $(FC_FLAGS) -o $(OBJ_MAIN_i)
$(OBJ_MAIN_j) : $(SRC_MAIN_j)
	$(CXX) $(CXX_FLAGS) -Wno-write-strings $(INCLUDE_GCOM) -c $(SRC_MAIN_j) -o $(OBJ_MAIN_j)
$(OBJ_MAIN_k) : $(SRC_MAIN_k) $(OBJ_UTILITY)
	$(FC) $(SRC_MAIN_k) $(EXTRA_FC_FLAGS) $(FC_FLAGS) $(qsaveOption) -o $(OBJ_MAIN_k)

$(OBJ_UTILITY) : $(SRC_UTILITY)
	cd $(lib_path) ; make
	$(FC) $(MAIN_path)/calib_ssmis_sub.f90 $(EXTRA_FC_FLAGS) \
	      $(FC_FLAGS) $(qsaveOption) -o $(MAIN_path)/calib_ssmis_sub.o

$(OBJ_MT) : $(SRC_MT) $(SRC_MT_HEAD) 
	cd $(lib_path_cpp) ; make



#---Dependence of the executable
$(EXE_1) : $(OBJ_MAIN_1)
	$(FL) $(OBJ_1) $(FL_FLAGS) $(EXE_1)
$(EXE_2) : $(OBJ_MAIN_2)
	$(FL) $(OBJ_2) $(FL_FLAGS) $(EXE_2)
$(EXE_3) : $(OBJ_MAIN_3)
	$(FL) $(OBJ_3) $(FL_FLAGS) $(EXE_3)
$(EXE_4) : $(OBJ_MAIN_4)
	$(FL) $(OBJ_4) $(FL_FLAGS) $(EXE_4)
$(EXE_5) : $(OBJ_MAIN_5)
	$(FL) $(OBJ_5) $(FL_FLAGS) $(EXE_5)
$(EXE_6) : $(OBJ_MAIN_6)
	$(FL) $(OBJ_6) $(FL_FLAGS) $(EXE_6)
$(EXE_7) : $(OBJ_MAIN_7)
	$(FL) $(OBJ_7) $(FL_FLAGS) $(EXE_7)
$(EXE_8) : $(OBJ_MAIN_8)
	$(FL) $(OBJ_8) $(FL_FLAGS) $(EXE_8)
$(EXE_a) : $(OBJ_MAIN_a)
	$(FL) $(OBJ_a) $(FL_FLAGS) $(EXE_a)
$(EXE_b) : $(OBJ_MAIN_b)
	$(CXX) -o $(EXE_b) $(OBJ_MAIN_b) $(LIBS_NPP) $(LDFLAGS_NPP)
$(EXE_c) : $(OBJ_MAIN_c)
	$(CXX) -o $(EXE_c) $(OBJ_MAIN_c) $(LIBS_NPP) $(LDFLAGS_NPP)
$(EXE_d) : $(OBJ_MAIN_d)
	$(FL) $(OBJ_d) $(FL_FLAGS_FY3) $(EXE_d)
$(EXE_e) : $(OBJ_MAIN_e)
	$(FL) $(OBJ_e) $(FL_FLAGS_TRMM) $(EXE_e)
$(EXE_f) : $(OBJ_f)
	$(CXX) -o $(EXE_f) $(OBJ_f) $(LIBS_MT) $(LDFLAGS_MT)
$(EXE_g) : $(OBJ_g)
	$(CXX) -o $(EXE_g) $(OBJ_g) $(LIBS_MT) $(LDFLAGS_MT)
$(EXE_h) : $(OBJ_MAIN_h)
	$(FL) $(OBJ_h) $(FL_FLAGS) $(EXE_h)
$(EXE_i) : $(OBJ_MAIN_i)
	$(FL) $(OBJ_i) $(FL_FLAGS) $(EXE_i)
$(EXE_j) : $(OBJ_MAIN_j)
	$(CXX) -o $(EXE_j) $(OBJ_MAIN_j) $(LIBS_GCOM) $(LDFLAGS_GCOM)
$(EXE_k) : $(OBJ_MAIN_k)
	$(FL) $(OBJ_k) $(FL_FLAGS) $(EXE_k)


# --------
# Clean up
# --------

clean:
	$(REMOVE) $(EXE_S) $(OBJ_MAIN_S) $(MAIN_path)/calib_ssmis_sub.o $(MAIN_path)/calib_ssmis_sub.mod
