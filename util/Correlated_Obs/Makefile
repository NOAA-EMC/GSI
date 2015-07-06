#==============================================================================
#
# Makefile for Compute_RadDiag_Stats program
#
#==============================================================================

# Define macros
include make.macros

# This makefile
MAKE_FILE = Makefile

# Executable file
EXE_FILE = cov_calc

# Source files to link
SRC_FILES = kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            RadDiag_Define.f90 RadDiag_Hdr_Define.f90 RadDiag_Data_Define.f90 \
            RadDiag_IO.f90 \
            pairs.f90 \
            obs_tools.f90

# Obj files used in link phase
OBJ_FILES = $(SRC_FILES:.f90=.o) \
            $(EXE_FILE).o

# Include and library definitions
#INCLUDES = -I$(HOME)/local/netcdf/include
#LIBRARIES = -L$(HOME)/local/netcdf/lib -lnetcdf
# Include and library definitions (for cirrus/stratus)
#INCLUDES = -I/nwprod/lib/sorc/netcdf/netcdf-3.5.0/include
#LIBRARIES = -L/nwprod/lib -lnetcdf_64

# Define common make targets (all, build, clean, install)
include make.common_targets

# Squeaky clean target
realclean: clean remove_links

# Source dependency lists
include make.dependencies

# Define default rules
include make.rules
