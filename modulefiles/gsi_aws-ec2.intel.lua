help([[
]])

prepend_path("MODULEPATH", "/opt/spack-stack/envs/ue-oneapi-2024.2.1/install/modulefiles/Core")
prepend_path("MODULEPATH", "/opt/modulefiles")

stack_oneapi_ver=os.getenv("stack_oneapi_ver") or "2024.2.1"
stack_impi_ver=os.getenv("stack_impi_ver") or "2021.13"
cmake_ver=os.getenv("cmake_ver") or "3.27.9"

load(pathJoin("stack-oneapi", stack_oneapi_ver))
load(pathJoin("stack-intel-oneapi-mpi", stack_impi_ver))
load(pathJoin("cmake", cmake_ver))

-- local stack_python_ver=os.getenv("stack_python_ver") or "3.11.7"
-- load(pathJoin("stack-python", stack_python_ver))

-- load("gsi_common")

local netcdf_c_ver=os.getenv("netcdf_c_ver") or "4.9.2"
local netcdf_fortran_ver=os.getenv("netcdf_fortran_ver") or "4.6.1"

local bufr_ver=os.getenv("bufr_ver") or "12.1.0"
local bacio_ver=os.getenv("bacio_ver") or "2.4.1"
local w3emc_ver=os.getenv("w3emc_ver") or "2.10.0"
local ip_ver=os.getenv("ip_ver") or "5.1.0"
local sigio_ver=os.getenv("sigio_ver") or "2.3.3"
local sfcio_ver=os.getenv("sfcio_ver") or "1.4.2"
local nemsio_ver=os.getenv("nemsio_ver") or "2.5.4"
local wrf_io_ver=os.getenv("wrf_io_ver") or "1.2.0"
local ncio_ver=os.getenv("ncio_ver") or "1.1.2"
local crtm_ver=os.getenv("crtm_ver") or "2.4.0.1"
local ncdiag_ver=os.getenv("ncdiag_ver") or "1.1.2"
-- local prod_util_ver=os.getenv("prod_util_ver") or "2.1.1"

load(pathJoin("netcdf-c", netcdf_c_ver))
load(pathJoin("netcdf-fortran", netcdf_fortran_ver))

load(pathJoin("bufr", bufr_ver))
load(pathJoin("bacio", bacio_ver))
load(pathJoin("w3emc", w3emc_ver))
load(pathJoin("ip", ip_ver))
load(pathJoin("sigio", sigio_ver))
load(pathJoin("sfcio", sfcio_ver))
load(pathJoin("nemsio", nemsio_ver))
load(pathJoin("wrf-io", wrf_io_ver))
load(pathJoin("ncio", ncio_ver))
load(pathJoin("crtm", crtm_ver))
load(pathJoin("gsi-ncdiag", ncdiag_ver))
-- load(pathJoin("prod_util", prod_util_ver))

pushenv("CFLAGS", "-xHOST")
pushenv("FFLAGS", "-xHOST")

local fix_base_path = os.getenv("GSI_FIX_BASE") or "/lustre/sharedGWdata/fix"
local crtm_fix_ver=os.getenv("crtm_fix_ver") or "2.4.0.2"
pushenv("GSI_BINARY_SOURCE_DIR", pathJoin(fix_base_path, "gsi/20251105"))
setenv("CRTM_FIX", pathJoin(pathJoin(fix_base_path, "crtm"), "v" .. crtm_fix_ver))

whatis("Description: GSI environment on NOAA Cloud with Intel Compilers")
