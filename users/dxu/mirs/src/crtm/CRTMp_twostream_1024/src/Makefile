#==============================================================================
#
# Makefile for CRTMp_twostream program
#
#==============================================================================

#-----------------------------------------------------------------------------
#                          -- Define macros --
#-----------------------------------------------------------------------------


#---Paths are read from a common file (used across the MIRS package)
paths_path = ../../../../setup
include $(paths_path)/paths


include make.macros


# ----------------
# Executable files
# ----------------

EXE_FILE = CRTMp_twostream


# -------------
# This makefile
# -------------

MAKEFILE = Makefile


# ------------
# Object files
# ------------

OBJ_CRTM_FILES = \
    TauCoeff_Define.o \
    TauCoeff_Binary_IO.o \
    CRTM_Module.o \
    CRTM_Parameters.o \
    CRTM_ChannelInfo_Define.o \
    CRTM_ChannelInfo.o \
    CRTM_SpcCoeff.o \
    CRTM_LifeCycle.o \
    CRTM_TauCoeff.o \
    SpcCoeff_Define.o \
    SpcCoeff_Binary_IO.o \
    mw_cloud_opt.o \
    Surface_Emissivity_Model.o \
    CRTM_Fastem3.o \
    mod_rttov_fastem4_coef.o \
    rttov_fastem4.o \
    absorber_profile.o \
    predictors.o \
    transmittance.o \
    sensor_planck_routines.o \
    radiance.o \
    RT_Twostream.o \
    RT_Twostream_Variable.o \
    K_matrix_module.o \
    forward_model.o \
    landem.o \
    SIceEM.o \
    MOD_SURFACE_COEF.o \
    Profile_Init.o \
    RTmodel_Init.o \
    $(EXE_FILE).o

OBJ_UTILITY_FILES = \
    type_kinds.o \
    file_utility.o \
    error_handler.o \
    Compare_Float_Numbers.o \
    Endian_Utility.o \
    Binary_File_Utility.o \
    constants.o \
    utility.o

OBJ_FILES=\
  ${OBJ_CRTM_FILES} \
  ${OBJ_UTILITY_FILES}

SRC_CRTM_FILES = ${OBJ_CRTM_FILES:.o=.f90}
SRC_CRTM_DIR=../CRTM

SRC_UTILITY_FILES = ${OBJ_UTILITY_FILES:.o=.f90}
SRC_UTILITY_DIR=../UTILITY

# -------------------------------
# Include and library definitions
# -------------------------------

#INCLUDES = -I$(HOME)/local/netcdf/include
#LIBRARIES = -L$(HOME)/local/netcdf/lib -lnetcdf

INCLUDES =
LIBRARIES =

# -----------------------
# Extra compilation flags
# -----------------------

EXTRA_FC_FLAGS = ${convEndOption}


#-----------------------------------------------------------------------------
#                          -- Define dependecies --
#-----------------------------------------------------------------------------



# ---------------------
# Make based on OS type
# ---------------------

all:
	@echo "OS type detected: "`uname -s`
	@case `uname -s` in \
	  "SunOS")    make -f $(MAKEFILE) the_program $(SUNOS_FLAGS) ;; \
	   "AIX")     make -f $(MAKEFILE) the_program $(AIX_FLAGS) ;; \
	   "IRIX64" ) make -f $(MAKEFILE) the_program $(IRIX64_FLAGS) ;; \
	   "Linux" )  make -f $(MAKEFILE) the_program $(Linux_FLAGS) ;; \
	   *) echo "This system is not supported" ;; \
       esac


# ---------------------
# Make the test program
# ---------------------

the_program: create_links $(OBJ_FILES)
	$(FL) $(OBJ_FILES) $(FL_FLAGS) $(EXE_FILE)


create_links:
	@for source_file in ${SRC_CRTM_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${SRC_CRTM_DIR}/$$source_file . ; \
	  fi ; \
	done
	@for source_file in ${SRC_UTILITY_FILES}; do \
	  if [ ! -f $$source_file ]; then \
	    $(LINK) ${SRC_UTILITY_DIR}/$$source_file . ; \
	  fi ; \
	done

ALL_LINKED_FILES = ${SRC_CRTM_FILES} \
                   ${SRC_UTILITY_FILES}

# --------
# Clean up
# --------

clean:
	$(REMOVE) $(OBJ_FILES) $(EXE_FILE) *.mod *.MOD *.stb *~

realclean: remove_links clean

remove_links:
	@for source_file in ${ALL_LINKED_FILES}; do \
	  $(REMOVE) $$source_file ; \
	done

# ---------------
# Dependency list
# ---------------

type_kinds.o : type_kinds.f90 
file_utility.o : file_utility.f90 
error_handler.o : error_handler.f90 file_utility.o
Endian_Utility.o : Endian_Utility.f90 type_kinds.o
Binary_File_Utility.o : Binary_File_Utility.f90 type_kinds.o error_handler.o file_utility.o Endian_Utility.o
Compare_Float_Numbers.o : Compare_Float_Numbers.f90 type_kinds.o error_handler.o

TauCoeff_Define.o :  TauCoeff_Define.f90 type_kinds.o error_handler.o Compare_Float_Numbers.o

TauCoeff_Binary_IO.o : TauCoeff_Binary_IO.f90 type_kinds.o error_handler.o file_utility.o Binary_File_Utility.o TauCoeff_Define.o

CRTM_Parameters.o : CRTM_Parameters.f90 type_kinds.o

CRTM_ChannelInfo_Define.o : CRTM_ChannelInfo_Define.f90 type_kinds.o error_handler.o

CRTM_ChannelInfo.o : CRTM_ChannelInfo.f90 type_kinds.o error_handler.o CRTM_ChannelInfo_Define.o

CRTM_SpcCoeff.o : CRTM_SpcCoeff.f90 type_kinds.o error_handler.o SpcCoeff_Define.o SpcCoeff_Binary_IO.o CRTM_Parameters.o

CRTM_TauCoeff.o : CRTM_TauCoeff.f90 type_kinds.o error_handler.o TauCoeff_Define.o TauCoeff_Binary_IO.o CRTM_Parameters.o

CRTM_LifeCycle.o : CRTM_LifeCycle.f90 error_handler.o CRTM_SpcCoeff.o CRTM_TauCoeff.o CRTM_ChannelInfo_Define.o CRTM_ChannelInfo.o mw_cloud_opt.o

SpcCoeff_Define.o : SpcCoeff_Define.f90 type_kinds.o error_handler.o Compare_Float_Numbers.o

SpcCoeff_Binary_IO.o : SpcCoeff_Binary_IO.f90 type_kinds.o error_handler.o file_utility.o SpcCoeff_Define.o Binary_File_Utility.o


mw_cloud_opt.o : mw_cloud_opt.f90 type_kinds.o error_handler.o Binary_File_Utility.o CRTM_Parameters.o

absorber_profile.o : absorber_profile.f90 type_kinds.o CRTM_Parameters.o

predictors.o : predictors.f90 type_kinds.o error_handler.o CRTM_Parameters.o 

transmittance.o : transmittance.f90 type_kinds.o CRTM_Parameters.o CRTM_TauCoeff.o

sensor_planck_routines.o : sensor_planck_routines.f90 type_kinds.o CRTM_Parameters.o CRTM_SpcCoeff.o

radiance.o : radiance.f90 type_kinds.o error_handler.o CRTM_Parameters.o CRTM_SpcCoeff.o sensor_planck_routines.o RT_Twostream.o

RT_Twostream.o : RT_Twostream.f90 type_kinds.o CRTM_Parameters.o RT_Twostream_Variable.o
Profile_Init.o : Profile_Init.f90 type_kinds.o
RTmodel_Init.o : RTmodel_Init.f90 type_kinds.o

RT_Twostream_Variable.o : RT_Twostream_Variable.f90 type_kinds.o CRTM_Parameters.o

K_matrix_module.o : K_matrix_module.f90 type_kinds.o error_handler.o CRTM_Parameters.o CRTM_SpcCoeff.o mw_cloud_opt.o Surface_Emissivity_Model.o CRTM_Fastem3.o absorber_profile.o predictors.o transmittance.o radiance.o RT_Twostream_Variable.o

forward_model.o : forward_model.f90 type_kinds.o error_handler.o CRTM_Parameters.o CRTM_SpcCoeff.o mw_cloud_opt.o Surface_Emissivity_Model.o CRTM_Fastem3.o absorber_profile.o predictors.o transmittance.o radiance.o RT_Twostream_Variable.o 

CRTM_Module.o : CRTM_Module.f90 CRTM_Parameters.o CRTM_ChannelInfo_Define.o forward_model.o K_matrix_module.o CRTM_LifeCycle.o

MOD_SURFACE_COEF.o : MOD_SURFACE_COEF.f90 type_kinds.o

constants.o : constants.f90

landem.o : landem.f90 constants.o

SIceEM.o : SIceEM.f90

Surface_Emissivity_Model.o : Surface_Emissivity_Model.f90 type_kinds.o error_handler.o MOD_SURFACE_COEF.o CRTM_Parameters.o landem.o SIceEM.o
CRTM_Fastem3.o : CRTM_Fastem3.f90 type_kinds.o CRTM_Parameters.o 
mod_rttov_fastem4_coef.o : mod_rttov_fastem4_coef.f90
rttov_fastem4.o : rttov_fastem4.f90 mod_rttov_fastem4_coef.o

utility.o : utility.f90

$(EXE_FILE).o : $(EXE_FILE).f90 type_kinds.o file_utility.o error_handler.o CRTM_Module.o


# -------------------------------
# Create tarball for distribution
# -------------------------------

TAR_FILE = $(EXE_FILE)_software.tar

tar:
	$(TARBALL_CREATE) $(TAR_FILE) ${OBJ_FILES:.o=.f90}
	$(TARBALL_APPEND) $(TAR_FILE) makefile
	$(TARBALL_APPEND) $(TAR_FILE) make.macros make.rules
	compress $(TAR_FILE)


install:
	cp $(EXE_FILE) ../exec
#-----------------------------------------------------------------------------
#                          -- Define default rules --
#-----------------------------------------------------------------------------

include make.rules

