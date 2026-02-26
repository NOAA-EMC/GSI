help([[
]])

setenv("LMOD_TMOD_FIND_FIRST","yes")
prepend_path("MODULEPATH", "/lustre/desc1/scratch/epicufsrt/contrib/modulefiles_extra")
prepend_path("MODULEPATH", "/glade/work/epicufsrt/contrib/spack-stack/derecho/spack-stack-1.9.2/envs/ue-oneapi-2024.2.1/install/modulefiles/Core")

local stack_oneapi_ver=os.getenv("stack_oneapi_ver") or "2024.2.1"
load(pathJoin("stack-oneapi", stack_oneapi_ver))

local stack_cray_mpich_ver=os.getenv("stack_cray_mpich_ver") or "8.1.29"
load(pathJoin("stack-cray-mpich", stack_cray_mpich_ver))

local python_ver=os.getenv("python_ver") or "3.11.7"
local cmake_ver=os.getenv("cmake_ver") or "3.27.9"

load(pathJoin("python", python_ver))
load(pathJoin("cmake", cmake_ver))

-- local oneapi_mkl_ver=os.getenv("oneapi_mkl_ver") or "2024.2.1"
-- load(pathJoin("intel-oneapi-mkl", oneapi_mkl_ver))
local crtm_fix_ver=os.getenv("crtm_fix_ver") or "2.4.0.2"

load("gsi_common")

local fix_base_path = os.getenv("GSI_FIX_BASE") or "/gpfs/csfs1/work/huangwei/GW-fix-data"
pushenv("GSI_BINARY_SOURCE_DIR", pathJoin(fix_base_path, "gsi/20251105"))
setenv("CRTM_FIX", pathJoin(pathJoin(fix_base_path, "crtm"), "v" .. crtm_fix_ver))

whatis("Description: GSI environment on NCAR derecho with Intel Compilers")
