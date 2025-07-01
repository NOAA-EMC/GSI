help([[
]])

prepend_path("MODULEPATH", "/ncrc/proj/epic/spack-stack/c6/spack-stack-1.9.2/envs/ue-intel-2023.2.0/install/modulefiles/Core")

local stack_python_ver=os.getenv("stack_python_ver") or "3.11.7"
local stack_intel_ver=os.getenv("stack_intel_ver") or "2023.2.0"
local stack_cray_mpich_ver=os.getenv("stack_cray_mpich_ver") or "8.1.30"
local cmake_ver=os.getenv("cmake_ver") or "3.27.9"

load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("stack-cray-mpich", stack_cray_mpich_ver))
load(pathJoin("stack-python", stack_python_ver))
load(pathJoin("cmake", cmake_ver))

load("gsi_common")

pushenv("GSI_BINARY_SOURCE_DIR", "/gpfs/f6/bil-fire8/world-shared/GSI_data/fix/gsi/20241022")

setenv("CC","cc")
setenv("FC","ftn")
setenv("CXX","CC")
pushenv("CRAYPE_LINK_TYPE","dynamic")

unload("cray-libsci")
whatis("Description: GSI environment on GaeaC6 with Intel Compilers")
