help([[
]])

unload("intel")
unload("cray-mpich")
unload("cray-python")
unload("darshan")

prepend_path("MODULEPATH", "/lustre/f2/dev/wpo/role.epic/contrib/spack-stack/spack-stack-1.4.1-c4/envs/unified-env/install/modulefiles/Core")
prepend_path("MODULEPATH", "/lustre/f2/pdata/esrl/gsd/spack-stack/modulefiles")

local stack_python_ver=os.getenv("stack_python_ver") or "3.9.12"
local stack_intel_ver=os.getenv("stack_intel_ver") or "2022.0.2"
local stack_cray_mpich_ver=os.getenv("stack_cray_mpich_ver") or "7.7.20"
local cmake_ver=os.getenv("cmake_ver") or "3.23.1"

load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("stack-cray-mpich", stack_cray_mpich_ver))
load(pathJoin("stack-python", stack_python_ver))
load(pathJoin("cmake", cmake_ver))

load("gsi_common")

local prod_util_ver=os.getenv("prod_util_ver") or "1.2.2"
load(pathJoin("prod-util", prod_util_ver))

-- Needed at runtime:
load("alps")

local MKLROOT="/opt/intel/oneapi/mkl/2022.0.2/"
prepend_path("LD_LIBRARY_PATH",pathJoin(MKLROOT,"lib/intel64"))
pushenv("MKLROOT", MKLROOT)

pushenv("GSI_BINARY_SOURCE_DIR", "/lustre/f2/dev/role.epic/contrib/GSI_data/fix/20240208")

setenv("CC","cc")
setenv("FC","ftn")
setenv("CXX","CC")
pushenv("CRAYPE_LINK_TYPE","dynamic")

whatis("Description: GSI environment on Gaea with Intel Compilers")

