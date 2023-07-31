help([[
]])

prepend_path("MODULEPATH", "/work/noaa/epic/role-epic/spack-stack/spack-stack-1.4.1/envs/unified-env/install/modulefiles/Core")
prepend_path("MODULEPATH", "/work/noaa/da/role-da/spack-stack/modulefiles")

local stack_python_ver=os.getenv("stack_python_ver") or "3.9.7"
local stack_intel_ver=os.getenv("stack_intel_ver") or "2022.0.2"
local stack_impi_ver=os.getenv("stack_impi_ver") or "2021.5.1"
local cmake_ver=os.getenv("cmake_ver") or "3.23.1"

load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("stack-intel-oneapi-mpi", stack_impi_ver))
load(pathJoin("stack-python", stack_python_ver))
load(pathJoin("cmake", cmake_ver))

load("gsi_common")

pushenv("CFLAGS", "-xHOST")
pushenv("FFLAGS", "-xHOST")

pushenv("GSI_BINARY_SOURCE_DIR", "/work/noaa/global/glopara/fix/gsi/20230601")

whatis("Description: GSI environment on Orion with Intel Compilers")
