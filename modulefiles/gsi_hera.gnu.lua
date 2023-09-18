help([[
]])

prepend_path("MODULEPATH", "/scratch1/NCEPDEV/nems/role.epic/spack-stack/spack-stack-1.4.1/envs/unified-env/install/modulefiles/Core")
prepend_path("MODULEPATH", "/scratch1/NCEPDEV/jcsda/jedipara/spack-stack/modulefiles")

local stack_python_ver=os.getenv("stack_python_ver") or "3.9.12"
local stack_gnu_ver=os.getenv("stack_gnu_ver") or "9.2.0"
local stack_openmpi_ver=os.getenv("stack_openmpi_ver") or "4.1.5"
local cmake_ver=os.getenv("cmake_ver") or "3.23.1"
local prod_util_ver=os.getenv("prod_util_ver") or "1.2.2"
local openblas_ver=os.getenv("openblas_ver") or "0.3.19"

load(pathJoin("stack-gcc", stack_gnu_ver))
load(pathJoin("stack-openmpi", stack_openmpi_ver))
load(pathJoin("stack-python", stack_python_ver))
load(pathJoin("cmake", cmake_ver))

load("gsi_common")

load(pathJoin("prod-util", prod_util_ver))
load(pathJoin("openblas", openblas_ver))

pushenv("GSI_BINARY_SOURCE_DIR", "/scratch1/NCEPDEV/global/glopara/fix/gsi/20230911")

whatis("Description: GSI environment on Hera with GNU Compilers")
