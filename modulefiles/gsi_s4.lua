help([[
]])

prepend_path("MODULEPATH", "/data/prod/jedi/spack-stack/spack-stack-1.5.1/envs/gsi-addon/install/modulefiles/Core")

local python_ver=os.getenv("python_ver") or "3.10.8"
local stack_intel_ver=os.getenv("stack_intel_ver") or "2021.5.0"
local stack_impi_ver=os.getenv("stack_impi_ver") or "2021.5.0"
local cmake_ver=os.getenv("cmake_ver") or "3.23.1"
local prod_util_ver=os.getenv("prod_util_ver") or "1.2.2"

load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("stack-intel-oneapi-mpi", stack_impi_ver))
load(pathJoin("python", python_ver))
load(pathJoin("cmake", cmake_ver))

load("gsi_common")
load(pathJoin("prod_util", prod_util_ver))

pushenv("CFLAGS", "-march=ivybridge")
pushenv("FFLAGS", "-march=ivybridge")

pushenv("GSI_BINARY_SOURCE_DIR", "/data/prod/glopara/fix/gsi/20230911")

whatis("Description: GSI environment on S4 with Intel Compilers")
