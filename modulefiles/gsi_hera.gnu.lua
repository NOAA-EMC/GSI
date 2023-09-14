help([[
]])

prepend_path("MODULEPATH", "/scratch1/NCEPDEV/nems/role.epic/hpc-stack/libs/gnu-9.2/modulefiles/stack")

local hpc_ver=os.getenv("hpc_ver") or "1.2.0"
local gnu_ver=os.getenv("gnu_ver") or "9.2.0"
local hpc_gnu_ver=os.getenv("hpc_gnu_ver") or "9.2"
local hpc_mpich_ver=os.getenv("hpc_mpich_ver") or "3.3.2"
local cmake_ver=os.getenv("cmake_ver") or "3.20.1"
local prod_util_ver=os.getenv("prod_util_ver") or "1.2.2"
local openblas_ver=os.getenv("openblas_ver") or "0.3.23"

load(pathJoin("hpc", hpc_ver))
load(pathJoin("gnu", gnu_ver))
load(pathJoin("hpc-gnu", hpc_gnu_ver))
load(pathJoin("hpc-mpich", hpc_mpich_ver))
load(pathJoin("cmake", cmake_ver))

load("gsi_common")

load(pathJoin("prod_util", prod_util_ver))
load(pathJoin("openblas", openblas_ver))

pushenv("GSI_BINARY_SOURCE_DIR", "/scratch1/NCEPDEV/global/glopara/fix/gsi/20230911")

whatis("Description: GSI environment on Hera with GNU Compilers")
