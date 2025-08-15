help([[
]])

prepend_path("MODULEPATH", "/apps/contrib/spack-stack/spack-stack-1.9.2/envs/ue-oneapi-2024.1.0/install/modulefiles/Core")

local stack_oneapi_ver=os.getenv("stack_oneapi_ver") or "2024.2.1"
local stack_intel_oneapi_mpi_ver=os.getenv("stack_intel_oneapi_mpi_ver") or "2021.13"
local intel_oneapi_mkl_ver=os.getenv("intel_oneapi_mkl_ver") or "2024.2.1"
local stack_python_ver=os.getenv("stack_python_ver") or "3.11.7"
local cmake_ver=os.getenv("cmake_ver") or "3.27.9"

load(pathJoin("stack-oneapi", stack_oneapi_ver))
load(pathJoin("stack-intel-oneapi-mpi", stack_intel_oneapi_mpi_ver))
load(pathJoin("intel-oneapi-mkl", intel_oneapi_mkl_ver))
load(pathJoin("stack-python", stack_python_ver))
load(pathJoin("cmake", cmake_ver))

load("gsi_common")

unload("intel-oneapi-mpi/2021.13.1")
load("intel-oneapi-mpi/2021.7.1")

pushenv("CFLAGS", "-xHOST")
pushenv("FFLAGS", "-xHOST")

setenv("CC","mpiicc")
setenv("CXX","mpiicpc")
setenv("FC","mpiifort")

pushenv("GSI_BINARY_SOURCE_DIR", "/work2/noaa/global/role-global/fix/gsi/20250529")

whatis("Description: GSI environment on Orion with Intel Compilers")
