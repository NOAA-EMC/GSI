help([[
]])

prepend_path("MODULEPATH", "/ncrc/proj/epic/spack-stack/c6/spack-stack-1.9.2/envs/ue-intel-2023.2.0/install/modulefiles/Core")

local stack_python_ver=os.getenv("stack_python_ver") or "3.11.7"
local stack_intel_ver=os.getenv("stack_intel_ver") or "2023.2.0"
local stack_cray_mpich_ver=os.getenv("stack_cray_mpich_ver") or "8.1.30"
local cmake_ver=os.getenv("cmake_ver") or "3.27.9"
local crtm_fix_ver=os.getenv("crtm_fix_ver") or "2.4.0.2"

load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("stack-cray-mpich", stack_cray_mpich_ver))
load(pathJoin("stack-python", stack_python_ver))
load(pathJoin("cmake", cmake_ver))

load("gsi_common")

pushenv("GSI_BINARY_SOURCE_DIR", "/gpfs/f6/drsa-precip3/world-shared/role.glopara/fix/gsi/20251105")
setenv("CRTM_FIX", pathJoin("/gpfs/f6/drsa-precip3/world-shared/role.glopara/fix/crtm", "v" .. crtm_fix_ver))

setenv("CC","cc")
setenv("FC","ftn")
setenv("CXX","CC")
pushenv("CRAYPE_LINK_TYPE","dynamic")

unload("cray-libsci")
whatis("Description: GSI environment on GaeaC6 with Intel Compilers")
