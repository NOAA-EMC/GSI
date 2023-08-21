help([[
]])

load("PrgEnv-intel")
load("intel")
load("craype")
load("cray-mpich")
load("cmake")

prepend_path("MODULEPATH", "/lfs/h1/emc/nceplibs/noscrub/Mark.Potts/spack-stack/spack-stack-1.4.1/envs/unified-dev-19/install/modulefiles/Core")
load("stack-intel")
load("stack-cray-mpich")
load("prod-util")
load("gsi_common_wcoss2")

setenv("CC", "cc")
setenv("CXX", "CC")
setenv("FC", "ftn")
pushenv("GSI_BINARY_SOURCE_DIR", "/lfs/h2/emc/global/noscrub/emc.global/FIX/fix/gsi/20230601")
whatis("Description: GSI environment on Hera with Intel Compilers")
