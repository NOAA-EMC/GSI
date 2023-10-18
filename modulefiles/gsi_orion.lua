help([[
]])

prepend_path("MODULEPATH", "/work/noaa/epic/role-epic/contrib/orion/miniconda3/modulefiles")
miniconda3_ver=os.getenv("miniconda3_ver") or "4.12.0"
load(pathJoin("miniconda3", miniconda3_ver))

prepend_path("MODULEPATH", "/work/noaa/epic/role-epic/contrib/orion/hpc-stack/intel-2022.1.2/modulefiles/stack")

local hpc_ver=os.getenv("hpc_ver") or "1.2.0"
local hpc_intel_ver=os.getenv("hpc_intel_ver") or "2022.1.2"
local hpc_impi_ver=os.getenv("hpc_impi_ver") or "2022.1.2"
local cmake_ver=os.getenv("cmake_ver") or "3.22.1"
local prod_util_ver=os.getenv("prod_util_ver") or "1.2.2"

load(pathJoin("hpc", hpc_ver))
load(pathJoin("hpc-intel", hpc_intel_ver))
load(pathJoin("hpc-impi", hpc_impi_ver))
load(pathJoin("cmake", cmake_ver))

load("gsi_common")
setenv("crtm_ROOT","/work/noaa/da/eliu/JEDI-GDAS/crtm_v2.4.1-jedi.1-intel2022/build")
setenv("crtm_VERSION","2.4.1-jedi.1")
setenv("CRTM_INC","/work/noaa/da/eliu/JEDI-GDAS/crtm_v2.4.1-jedi.1-intel2022/build/module")
setenv("CRTM_LIB","/work/noaa/da/eliu/JEDI-GDAS/crtm_v2.4.1-jedi.1-intel2022/build/lib/libcrtm_static.a")
setenv("CRTM_FIX","/work/noaa/da/eliu/JEDI-GDAS/crtm_v2.4.1-jedi.1-fix/Little_Endian")
whatis("Name: crtm")
whatis("Version: 2.4.1-jedi.1")
whatis("Category: library")
whatis("Description: crtm library")

load(pathJoin("prod_util", prod_util_ver))

pushenv("CFLAGS", "-xHOST")
pushenv("FFLAGS", "-xHOST")

pushenv("GSI_BINARY_SOURCE_DIR", "/work/noaa/global/glopara/fix/gsi/20230911")

whatis("Description: GSI environment on Orion with Intel Compilers")
