help([[
]])

prepend_path("MODULEPATH", "/contrib/spack-stack/spack-stack-2.1.1/envs/ue-oneapi-2025.3.1/modules/Core")

local stack_oneapi_ver=os.getenv("stack_oneapi_ver") or "2025.3.1"
local stack_impi_ver=os.getenv("stack_impi_ver") or "2021.17"
local oneapi_mkl_ver=os.getenv("oneapi_mkl_ver") or "2025.3"
local cmake_ver=os.getenv("cmake_ver") or "3.31.8"
local crtm_fix_ver=os.getenv("crtm_fix_ver") or "3.1.2"

load(pathJoin("stack-intel-oneapi-compilers", stack_oneapi_ver))
load(pathJoin("stack-intel-oneapi-mpi", stack_impi_ver))
load(pathJoin("intel-oneapi-mkl", oneapi_mkl_ver))
load(pathJoin("cmake", cmake_ver))

load("gsi_common")

pushenv("GSI_BINARY_SOURCE_DIR", "/scratch3/NCEPDEV/global/role.glopara/fix/gsi/20251105")
setenv("CRTM_FIX", "/scratch3/NCEPDEV/da/Andrew.Collard/git/CRTMv3_GSI/fix_LittleEndian")

whatis("Description: GSI environment on Ursa with Intel Compilers")
