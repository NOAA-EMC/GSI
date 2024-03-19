help([[
]])

prepend_path("MODULEPATH", "/scratch1/NCEPDEV/nems/role.epic/spack-stack/spack-stack-1.6.0/envs/gsi-addon-dev-rocky8/install/modulefiles/Core")

local python_ver=os.getenv("python_ver") or "3.11.6"
local stack_intel_ver=os.getenv("stack_intel_ver") or "2021.5.0"
local stack_impi_ver=os.getenv("stack_impi_ver") or "2021.5.1"
local cmake_ver=os.getenv("cmake_ver") or "3.23.1"
local prod_util_ver=os.getenv("prod_util_ver") or "2.1.1"

load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("stack-intel-oneapi-mpi", stack_impi_ver))
load(pathJoin("python", python_ver))
load(pathJoin("cmake", cmake_ver))

load("gsi_common")
load(pathJoin("prod_util", prod_util_ver))

unload("crtm/2.4.0.1")
setenv("crtm_ROOT","/scratch1/NCEPDEV/da/Emily.Liu/JEDI-GDAS/crtm-v2.4.1-jedi.1-intel/build")
setenv("crtm_VERSION","2.4.1-jedi.1")
setenv("CRTM_INC","/scratch1/NCEPDEV/da/Emily.Liu/JEDI-GDAS/crtm-v2.4.1-jedi.1-intel/build/module/crtm/Intel/2021.5.0.20211109")
setenv("CRTM_LIB","/scratch1/NCEPDEV/da/Emily.Liu/JEDI-GDAS/crtm-v2.4.1-jedi.1-intel/build/lib/libcrtm_static.a")
setenv("CRTM_FIX","/scratch1/NCEPDEV/da/Cory.R.Martin/GDASApp/fix/crtm/2.4.0")

pushenv("CFLAGS", "-xHOST")
pushenv("FFLAGS", "-xHOST")

pushenv("GSI_BINARY_SOURCE_DIR", "/scratch1/NCEPDEV/global/glopara/fix/gsi/20240208")

whatis("Description: GSI environment on Hera with Intel Compilers")
