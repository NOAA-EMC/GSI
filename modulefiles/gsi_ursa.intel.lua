help([[
]])

prepend_path("MODULEPATH", "/contrib/spack-stack/spack-stack-1.9.2/envs/ue-oneapi-2024.2.1/install/modulefiles/Core")

local stack_oneapi_ver=os.getenv("stack_oneapi_ver") or "2024.2.1"
local stack_impi_ver=os.getenv("stack_impi_ver") or "2021.13"
local oneapi_mkl_ver=os.getenv("oneapi_mkl_ver") or "2024.2.1"
local stack_python_ver=os.getenv("stack_python_ver") or "3.11.7"
local cmake_ver=os.getenv("cmake_ver") or "3.27.9"
local crtm_fix_ver=os.getenv("crtm_fix_ver") or "2.4.0.2"

load(pathJoin("stack-oneapi", stack_oneapi_ver))
load(pathJoin("stack-intel-oneapi-mpi", stack_impi_ver))
load(pathJoin("intel-oneapi-mkl", oneapi_mkl_ver))
load(pathJoin("stack-python", stack_python_ver))
load(pathJoin("cmake", cmake_ver))

load("gsi_common")

pushenv("GSI_BINARY_SOURCE_DIR", "/scratch3/NCEPDEV/global/role.glopara/fix/gsi/20251105")
setenv("CRTM_FIX", pathJoin("/scratch3/NCEPDEV/global/role.glopara/fix/crtm", "v" .. crtm_fix_ver))

whatis("Description: GSI environment on Ursa with Intel Compilers")
