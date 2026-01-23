help([[
]])

prepend_path("MODULEPATH", "/opt/spack-stack/spack-stack-1.9.2/envs/unified-env/install/modulefiles/Core")

local stack_oneapi_ver=os.getenv("stack_oneapi_ver") or "2024.2.0"
local stack_intel_oneapi_mpi_ver=os.getenv("stack_intel_oneapi_mpi_ver") or "2021.13"
local cmake_ver=os.getenv("cmake_ver") or "3.27.9"

load(pathJoin("stack-oneapi", stack_oneapi_ver))
load(pathJoin("stack-intel-oneapi-mpi", stack_intel_oneapi_mpi_ver))
load(pathJoin("cmake", cmake_ver))

load("gsi_common")

pushenv("CFLAGS", "-xHOST")
pushenv("FFLAGS", "-xHOST")

-- pushenv("GSI_BINARY_SOURCE_DIR", "/contrib/global-workflow-shared-data/fix/gsi/20250529")
pushenv("GSI_BINARY_SOURCE_DIR", "/scratch3/NCEPDEV/global/role.glopara/fix/gsi/20251105")

whatis("Description: GSI environment in a container with Intel Compilers")
