help([[
]])

prepend_path("MODULEPATH", "/ncrc/proj/epic/spack-stack/c6/spack-stack-2.1.1/envs/ue-oneapi-2025.2.1/modules/Core")

local python_ver=os.getenv("python_ver") or "3.11"
local stack_intel_ver=os.getenv("stack_intel_ver") or "2025.2.1"
local stack_cray_mpich_ver=os.getenv("stack_cray_mpich_ver") or "8.1.32"
local cmake_ver=os.getenv("cmake_ver") or "3.31.8"
local crtm_fix_ver=os.getenv("crtm_fix_ver") or "3.1.2.0"

load(pathJoin("stack-intel-oneapi-compilers", stack_intel_ver))
load(pathJoin("stack-cray-mpich", stack_cray_mpich_ver))
load(pathJoin("python", python_ver))
load(pathJoin("cmake", cmake_ver))

load("gsi_common")

pushenv("GSI_BINARY_SOURCE_DIR", "/gpfs/f6/drsa-precip3/world-shared/role.glopara/fix/gsi/20251105")
setenv("CRTM_FIX", "/gpfs/f6/drsa-precip3/world-shared/role.glopara/fix/crtm/fix_LittleEndian")

setenv("CC","cc")
setenv("FC","ftn")
setenv("CXX","CC")
pushenv("CRAYPE_LINK_TYPE","dynamic")

unload("cray-libsci")
whatis("Description: GSI environment on GaeaC6 with Intel Compilers")
