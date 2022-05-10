help([[
Load common modules to build GSI on all machines
]])

local netcdf_ver=os.getenv("netcdf_ver") or "4.7.4"

local bufr_ver=os.getenv("bufr_ver") or "11.5.0"
local bacio_ver=os.getenv("bacio_ver") or "2.4.1"
local w3emc_ver=os.getenv("w3emc_ver") or "2.9.1"
local sp_ver=os.getenv("sp_ver") or "2.3.3"
local ip_ver=os.getenv("ip_ver") or "3.3.3"
local sigio_ver=os.getenv("sigio_ver") or "2.3.2"
local sfcio_ver=os.getenv("sfcio_ver") or "1.4.1"
local nemsio_ver=os.getenv("nemsio_ver") or "2.5.2"
local wrf_io_ver=os.getenv("wrf_io_ver") or "1.2.0"
local ncio_ver=os.getenv("ncio_ver") or "1.0.0"
local crtm_ver=os.getenv("crtm_ver") or "2.3.0"

load(pathJoin("netcdf", netcdf_ver))

load(pathJoin("bufr", bufr_ver))
load(pathJoin("bacio", bacio_ver))
load(pathJoin("w3emc", w3emc_ver))
load(pathJoin("sp", sp_ver))
load(pathJoin("ip", ip_ver))
load(pathJoin("sigio", sigio_ver))
load(pathJoin("sfcio", sfcio_ver))
load(pathJoin("nemsio", nemsio_ver))
load(pathJoin("wrf_io", wrf_io_ver))
load(pathJoin("ncio", ncio_ver))
load(pathJoin("crtm", crtm_ver))

