help([[
]])

prepend_path("MODULEPATH", "/contrib/spack-stack-rocky8/spack-stack-1.6.0/envs/gsi-addon-env/install/modulefiles/Core")
prepend_path("MODULEPATH", "/apps/modules/modulefiles")

local stack_intel_ver=os.getenv("stack_intel_ver") or "2021.10.0"
local stack_impi_ver=os.getenv("stack_impi_ver") or "2021.10.0"

load("gnu")
load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("stack-intel-oneapi-mpi", stack_impi_ver))
unload("gnu")

load("gsi_common")

local cmake_ver=os.getenv("cmake_ver") or "3.23.1"
local python_ver=os.getenv("python_ver") or "3.11.6"
local prod_util_ver=os.getenv("prod_util_ver") or "2.1.1"

load(pathJoin("cmake", cmake_ver))
load(pathJoin("python", python_ver))
load(pathJoin("prod_util", prod_util_ver))

pushenv("CFLAGS", "-xHOST")
pushenv("FFLAGS", "-xHOST")

pushenv("GSI_BINARY_SOURCE_DIR", "/contrib/global-workflow-shared-data/fix/gsi/20241022")

whatis("Description: GSI environment on NOAA Cloud with Intel Compilers")
