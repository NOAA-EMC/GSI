help([[
]])

prepend_path("MODULEPATH", "/apps/contrib/spack-stack/spack-stack-1.9.1/envs/ue-oneapi-2024.1.0/install/modulefiles/Core")

local stack_python_ver=os.getenv("stack_python_ver") or "3.11.6"
local stack_intel_ver=os.getenv("stack_intel_ver") or "2024.2.1"
local stack_impi_ver=os.getenv("stack_impi_ver") or "2021.13"
local cmake_ver=os.getenv("cmake_ver") or "3.27.9"

load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("stack-intel-oneapi-mpi", stack_impi_ver))
load(pathJoin("python", stack_python_ver))
load(pathJoin("cmake", cmake_ver))

load("gsi_common")
load("intel-oneapi-mkl/2024.2.1")

pushenv("CFLAGS", "-xHOST")
pushenv("FFLAGS", "-xHOST")

pushenv("GSI_BINARY_SOURCE_DIR", "/work2/noaa/global/role-global/fix/gsi/20241022")

whatis("Description: GSI environment on Orion with Intel Compilers")
