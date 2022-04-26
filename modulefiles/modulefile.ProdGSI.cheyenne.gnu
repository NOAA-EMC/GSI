#%Module######################################################################
# NOAA-EMC/GSI
#_____________________________________________________
#proc ModulesHelp { } {
#puts stderr "Set environment variables for NOAA-EMC/GSI"
#puts stderr "This module initializes the environment "
#puts stderr "for the Intel Compiler Suite $version\n"
##}
#module-whatis " NOAA-EMC/GSI whatis description"

module load cmake/3.22.0
module load python/3.7.9
module load ncarenv/1.3
module load gnu/10.1.0
module load mpt/2.22
module load ncarcompilers/0.5.0
module unload netcdf

module use /glade/p/ral/jntp/GMTB/tools/hpc-stack-v1.2.0/modulefiles/stack
module load hpc/1.2.0
module load hpc-gnu/10.1.0
module load hpc-mpt/2.22

setenv MKLROOT /glade/u/apps/opt/intel/2021.2/mkl/latest

module load modulefile.ProdGSI.common

