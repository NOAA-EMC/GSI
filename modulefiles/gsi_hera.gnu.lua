help([[
]])

prepend_path("MODULEPATH", "/scratch2/NCEPDEV/nwprod/hpc-stack/libs/hpc-stack/modulefiles/stack")

local hpc_ver=os.getenv("hpc_ver") or "1.1.0"
local hpc_gnu_ver=os.getenv("hpc_gnu_ver") or "9.2.0"
local hpc_mpich_ver=os.getenv("hpc_mpich_ver") or "3.3.2"
local cmake_ver=os.getenv("cmake_ver") or "3.20.1"
local prod_util_ver=os.getenv("prod_util_ver") or "1.2.2"

load(pathJoin("hpc", hpc_ver))
load(pathJoin("hpc-gnu", hpc_gnu_ver))
load(pathJoin("hpc-mpich", hpc_mpich_ver))
load(pathJoin("cmake", cmake_ver))

load("gsi_common")

load(pathJoin("prod_util", prod_util_ver))

pushenv("MKLROOT", "/apps/oneapi/mkl/2022.0.2")

whatis("Description: GSI environment on Hera with GNU Compilers")
