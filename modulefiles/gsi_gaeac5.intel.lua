help([[
]])

prepend_path("MODULEPATH", "/ncrc/proj/epic/spack-stack/spack-stack-1.6.0/envs/gsi-addon-dev/install/modulefiles/Core")

local stack_python_ver=os.getenv("stack_python_ver") or "3.11.6"
local stack_intel_ver=os.getenv("stack_intel_ver") or "2023.2.0"
local stack_cray_mpich_ver=os.getenv("stack_cray_mpich_ver") or "8.1.28"
local cmake_ver=os.getenv("cmake_ver") or "3.23.1"
local prod_util_ver=os.getenv("prod_util_ver") or "2.1.1"

load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("stack-cray-mpich", stack_cray_mpich_ver))
load(pathJoin("stack-python", stack_python_ver))
load(pathJoin("cmake", cmake_ver))

load("gsi_common")
load(pathJoin("prod_util", prod_util_ver))

local MKLROOT="/opt/intel/oneapi/mkl/2022.0.2/"
prepend_path("LD_LIBRARY_PATH",pathJoin(MKLROOT,"lib/intel64"))
pushenv("MKLROOT", MKLROOT)

pushenv("GSI_BINARY_SOURCE_DIR", "/gpfs/f5/ufs-ard/world-shared/GSI_data/fix/gsi/20240208")

setenv("CC","cc")
setenv("FC","ftn")
setenv("CXX","CC")
pushenv("CRAYPE_LINK_TYPE","dynamic")

unload("cray-libsci")
whatis("Description: GSI environment on Gaea with Intel Compilers")
