help([[
]])

prepend_path("MODULEPATH", "/opt/intel/oneapi/mpi/2021.18/etc/modulefiles/mpi")
--prepend_path("MODULEPATH", "/lfs/work/alexander_richert/stack/spack-stack/envs/nco-core-gcc-11.5.0/modules_flat/Core")
--prepend_path("MODULEPATH", "/lfs/work/alexander_richert/stack/spack-stack/envs/nco-sci-intel-2021.10.0/modules_flat/Core")
prepend_path("MODULEPATH", "/lfs/work/alexander_richert/stack/spack-stack/envs/g2-3.4.5-oneapi-2026.1.0-addon/modules_flat/Core")
--prepend_path("MODULEPATH", ": /lfs/work/alexander_richert/stack/spack-stack/envs/crtm-2.4.0.1-byterecl-oneapi-2026.1.0-addon/modules/flat/Core")

load("2021.18")
load("cmake/3.31.11")
load("python/3.11.15")

local netcdf_c_ver=os.getenv("netcdf_c_ver") or "4.9.2"
local netcdf_fortran_ver=os.getenv("netcdf_fortran_ver") or "4.6.1"

local bufr_ver=os.getenv("bufr_ver") or "12.3.0"
local bacio_ver=os.getenv("bacio_ver") or "2.4.1"
local w3emc_ver=os.getenv("w3emc_ver") or "2.13.0"
local ip_ver=os.getenv("ip_ver") or "5.4.0"
local sigio_ver=os.getenv("sigio_ver") or "2.3.3"
local sfcio_ver=os.getenv("sfcio_ver") or "1.4.2"
local nemsio_ver=os.getenv("nemsio_ver") or "2.5.5"
local wrf_io_ver=os.getenv("wrf_io_ver") or "1.3.0"
local ncio_ver=os.getenv("ncio_ver") or "1.1.2"
local crtm_ver=os.getenv("crtm_ver") or "2.4.0.1"
local crtm_fix_ver=os.getenv("crtm_fix_ver") or "2.4.0.1_emc"
local ncdiag_ver=os.getenv("ncdiag_ver") or "1.1.3"

load(pathJoin("netcdf-c", netcdf_c_ver))
load(pathJoin("netcdf-fortran", netcdf_fortran_ver))

load("python-venv/1.0")
load("py-setuptools/73.0.1")
load("py-numpy/1.26.4")
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
load(pathJoin("crtm-fix", crtm_fix_ver))

load(pathJoin("gsi-ncdiag", ncdiag_ver))

load("openblas/0.3.33")

setenv("CC","mpiicx")
setenv("CXX","mpiicpx")
setenv("FC","mpiifx")

local gsi_binary_source_dir=os.getenv("GSI_BINARY_SOURCE_DIR") or "/home/russ_treadon_hpc_noaa_gov/fix/gsi/20260224"
pushenv("GSI_BINARY_SOURCE_DIR", gsi_binary_source_dir)

whatis("Description: GSI environment on Nimbus with Intel Compilers")
