help([[
Load common modules to build GSI on all machines
]])

local netcdf_ver=os.getenv("netcdf_ver") or "4.7.4"

local bufr_ver=os.getenv("bufr_ver") or "11.7.0"
local bacio_ver=os.getenv("bacio_ver") or "2.4.1"
local w3emc_ver=os.getenv("w3emc_ver") or "2.9.1"
local sp_ver=os.getenv("sp_ver") or "2.3.3"
local ip_ver=os.getenv("ip_ver") or "3.3.3"
local sigio_ver=os.getenv("sigio_ver") or "2.3.2"
local sfcio_ver=os.getenv("sfcio_ver") or "1.4.1"
local nemsio_ver=os.getenv("nemsio_ver") or "2.5.2"
local wrf_io_ver=os.getenv("wrf_io_ver") or "1.2.0"
local ncio_ver=os.getenv("ncio_ver") or "1.1.2"
local crtm_ver=os.getenv("crtm_ver") or "2.4.0"
local ncdiag_ver=os.getenv("ncdiag_ver") or "1.1.1"

load("netcdf-c")
load("netcdf-fortran")

load("bufr/11.7.0")
load("bacio")
load("w3emc")
load("sp")
load("ip/3.3.3")
load("sigio")
load("sfcio")
load("nemsio")
load("wrf-io")
load("ncio")
load("crtm")
load(pathJoin("gsi-ncdiag",ncdiag_ver))

