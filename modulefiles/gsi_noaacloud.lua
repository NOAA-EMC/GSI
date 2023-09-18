help([[
]])

prepend_path("MODULEPATH", "/contrib/EPIC/spack-stack/spack-stack-1.4.1/envs/unified-env/install/modulefiles/Core")
prepend_path("MODULEPATH", "/contrib/spack-stack/modulefiles/core")

local stack_intel_ver=os.getenv("stack_intel_ver") or "2021.3.0"
local stack_impi_ver=os.getenv("stack_impi_ver") or "2021.3.0"
local cmake_ver=os.getenv("cmake_ver") or "3.23.1"
local stack_python_ver=os.getenv("stack_python_ver") or "3.9.12"
local prod_util_ver=os.getenv("stack_python_ver") or "1.2.2"

load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("stack-intel-oneapi-mpi", stack_impi_ver))
load(pathJoin("cmake", cmake_ver))
load(pathJoin("stack-python", stack_python_ver))

load("gsi_common")
load(pathJoin("prod-util", prod_util_ver))

pushenv("GSI_BINARY_SOURCE_DIR", "/contrib/EPIC/GSI_data/fix/20230601")
whatis("Description: GSI environment on Cloud with Intel Compilers")
