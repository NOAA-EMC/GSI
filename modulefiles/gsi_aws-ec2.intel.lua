help([[
]])

prepend_path("MODULEPATH", "/opt/spack-stack/envs/ue-oneapi-2024.2.1/install/modulefiles/Core")
prepend_path("MODULEPATH", "/opt/modulefiles")

local stack_oneapi_ver=os.getenv("stack_oneapi_ver") or "2024.2.1"
local stack_impi_ver=os.getenv("stack_impi_ver") or "2021.13"
local cmake_ver=os.getenv("cmake_ver") or "3.27.9"

load(pathJoin("stack-oneapi", stack_oneapi_ver))
load(pathJoin("stack-intel-oneapi-mpi", stack_impi_ver))
load(pathJoin("cmake", cmake_ver))

load("gsi_common")

pushenv("CFLAGS", "-xHOST")
pushenv("FFLAGS", "-xHOST")

local fix_base_path = os.getenv("GSI_FIX_BASE") or "/lustre/global/data/fix"
local crtm_fix_ver=os.getenv("crtm_fix_ver") or "2.4.0.2"
pushenv("GSI_BINARY_SOURCE_DIR", pathJoin(fix_base_path, "gsi/20251105"))
setenv("CRTM_FIX", pathJoin(pathJoin(fix_base_path, "crtm"), "v" .. crtm_fix_ver))

whatis("Description: GSI environment on native AWS with Intel Compilers")
