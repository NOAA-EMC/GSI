#%Module######################################################################
# NOAA-EMC/GSI
#_____________________________________________________
#proc ModulesHelp { } {
#puts stderr "Set environment variables for NOAA-EMC/GSI"
#puts stderr "This module initializes the environment "
#puts stderr "for the Intel Compiler Suite $version\n"
##}
#module-whatis " NOAA-EMC/GSI whatis description"

setenv CRTM_FIX /scratch2/NCEPDEV/nwprod/NCEPLIBS/fix/crtm_v2.3.0

# Load cmake
module use -a /contrib/cmake/modulefiles
module load cmake/3.20.1

# Load hpc-stack
module use /scratch2/NCEPDEV/nwprod/hpc-stack/libs/hpc-stack/modulefiles/stack
module load hpc/1.1.0

# Load GNU compiler and mpi
module load hpc-gnu/9.2.0
module load hpc-mpich/3.3.2

# Use Intel MKL
setenv MKLROOT "/apps/oneapi/mkl/2022.0.2"

# Load the dependency libraries
module load modulefile.ProdGSI.common
