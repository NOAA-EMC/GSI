help([[
]])

prepend_path("MODULEPATH", "/lfs/h1/emc/nceplibs/noscrub/spack-stack/spack-stack-1.4.1/envs/unified-env/install/modulefiles/Core")

load("stack-intel")
load("stack-cray-mpich")
load("stack-python")

--Avoid production installations; use spack-stack only:
remove_path("MODULEPATH", "/apps/ops/prod/libs/modulefiles/compiler/intel/19.1.3.304")
remove_path("MODULEPATH", "/apps/ops/prod/libs/modulefiles/mpi/intel/19.1.3.304/cray-mpich/8.1.4")
remove_path("MODULEPATH", "/apps/ops/prod/libs/modulefiles/mpi/intel/19.1.3.304/cray-mpich/8.1.7")
remove_path("MODULEPATH", "/apps/prod/lmodules/intel/19.1.3.304")
remove_path("MODULEPATH", "/apps/prod/lmodules/INTEL_cray_mpich/19.1.3.304/cray-mpich/8.1.4")

load("cmake")

load("gsi_common")

pushenv("GSI_BINARY_SOURCE_DIR", "/lfs/h2/emc/global/noscrub/emc.global/FIX/fix/gsi/20230601")

whatis("Description: GSI environment on WCOSS2")
