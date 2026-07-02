help([[
]])

local PrgEnv_intel_ver=os.getenv("PrgEnv_intel_ver") or "8.1.0"
local intel_ver=os.getenv("intel_ver") or "19.1.3.304"
local craype_ver=os.getenv("craype_ver") or "2.7.8"
local cray_mpich_ver=os.getenv("cray_mpich_ver") or "8.1.7"
local cmake_ver= os.getenv("cmake_ver") or "3.20.2"
local python_ver=os.getenv("python_ver") or "3.8.6"

local netcdf_ver=os.getenv("netcdf_ver") or "4.7.4"
local bufr_ver=os.getenv("bufr_ver") or "12.1.0"
local bacio_ver=os.getenv("bacio_ver") or "2.4.1"
local w3emc_ver=os.getenv("w3emc_ver") or "2.9.2"
local ip_ver=os.getenv("ip_ver") or "5.1.0"
local sigio_ver=os.getenv("sigio_ver") or "2.3.2"
local sfcio_ver=os.getenv("sfcio_ver") or "1.4.1"
local nemsio_ver=os.getenv("nemsio_ver") or "2.5.4"
local wrf_io_ver=os.getenv("wrf_io_ver") or "1.2.0"
local ncio_ver=os.getenv("ncio_ver") or "1.1.2"
local crtm_ver=os.getenv("crtm_ver") or "2.4.0.1"
local crtm_fix_ver=os.getenv("crtm_fix_ver") or "2.4.0.2"
local ncdiag_ver=os.getenv("ncdiag_ver") or "1.1.1"

load(pathJoin("PrgEnv-intel", PrgEnv_intel_ver))
load(pathJoin("intel", intel_ver))
load(pathJoin("craype", craype_ver))
load(pathJoin("cray-mpich", cray_mpich_ver))
load(pathJoin("cmake", cmake_ver))
load(pathJoin("python", python_ver))

load(pathJoin("netcdf", netcdf_ver))
load(pathJoin("bufr", bufr_ver))
load(pathJoin("bacio", bacio_ver))
load(pathJoin("w3emc", w3emc_ver))
--load(pathJoin("ip", ip_ver))
-- Temporarily define IP's paths here.
-- TODO when testing is complete, request an official installation in https://github.com/NOAA-EMC/WCOSS2-requests/issues/11
pushenv("ip_ROOT", pathJoin("/lfs/h2/emc/nceplibs/noscrub/hpc-stack/libs/hpc-stack/intel-19.1.3.304/ip", ip_ver))
pushenv("IP_INC4", pathJoin("/lfs/h2/emc/nceplibs/noscrub/hpc-stack/libs/hpc-stack/intel-19.1.3.304/ip", ip_ver, "include_4"))
pushenv("IP_INCd", pathJoin("/lfs/h2/emc/nceplibs/noscrub/hpc-stack/libs/hpc-stack/intel-19.1.3.304/ip", ip_ver, "include_d"))
pushenv("IP_LIB4", pathJoin("/lfs/h2/emc/nceplibs/noscrub/hpc-stack/libs/hpc-stack/intel-19.1.3.304/ip", ip_ver, "lib64/libip_4.a"))
pushenv("IP_LIBd", pathJoin("/lfs/h2/emc/nceplibs/noscrub/hpc-stack/libs/hpc-stack/intel-19.1.3.304/ip", ip_ver, "lib64/libip_d.a"))
pushenv("ip_VERSION", ip_ver)

load(pathJoin("sigio", sigio_ver))
load(pathJoin("sfcio", sfcio_ver))
load(pathJoin("nemsio", nemsio_ver))
load(pathJoin("wrf_io", wrf_io_ver))
load(pathJoin("ncio", ncio_ver))
--load(pathJoin("crtm", crtm_ver))
load(pathJoin("ncdiag",ncdiag_ver))

-- Lastly, load CRTM from the EMC location
append_path("MODULEPATH", "/lfs/h1/emc/nceplibs/noscrub/hpc-stack/libs/hpc-stack/modulefiles/compiler/intel/19.1.3.304")
load(pathJoin("crtm", crtm_ver))

pushenv("GSI_BINARY_SOURCE_DIR", "/lfs/h2/emc/global/noscrub/emc.global/FIX/fix/gsi/20251105")
setenv("CRTM_FIX", pathJoin("/lfs/h2/emc/global/noscrub/emc.global/FIX/fix/crtm", "v" .. crtm_fix_ver))

whatis("Description: GSI environment on WCOSS2 Acorn")
