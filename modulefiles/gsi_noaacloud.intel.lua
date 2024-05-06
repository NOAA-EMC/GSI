help([[
]])

prepend_path("MODULEPATH", "/contrib/spack-stack/spack-stack-1.6.0/envs/gsi-addon-env/install/modulefiles/Core")

local python_ver=os.getenv("python_ver") or "3.10.13"
local stack_intel_ver=os.getenv("stack_intel_ver") or "2021.3.0"
local stack_impi_ver=os.getenv("stack_impi_ver") or "2021.3.0"
local cmake_ver=os.getenv("cmake_ver") or "3.23.1"
local prod_util_ver=os.getenv("prod_util_ver") or "2.1.1"

load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("stack-intel-oneapi-mpi", stack_impi_ver))
load(pathJoin("python", python_ver))
load(pathJoin("cmake", cmake_ver))

load("gsi_common")
load(pathJoin("prod_util", prod_util_ver))

pushenv("CFLAGS", "-xHOST")
pushenv("FFLAGS", "-xHOST")

pushenv("GSI_BINARY_SOURCE_DIR", "/contrib/Wei.Huang/data/hack-orion/fix/gsi/20240208")

whatis("Description: GSI environment on NOAA Cloud with Intel Compilers")
