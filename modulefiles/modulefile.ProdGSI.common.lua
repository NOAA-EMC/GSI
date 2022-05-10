help([[
Load common modules to build GSI on all machines
]])

prod_util_ver=os.getenv("prod_util_ver") or "1.2.2"

netcdf_ver=os.getenv("netcdf_ver") or "4.7.4"

bufr_ver=os.getenv("bufr_ver") or "11.5.0"
bacio_ver=os.getenv("bacio_ver") or "2.4.1"
w3emc_ver=os.getenv("w3emc_ver") or "2.9.1"
sp_ver=os.getenv("sp_ver") or "2.3.3"
sp_ver=os.getenv("ip_ver") or "3.3.3"
sigio_ver=os.getenv("sigio_ver") or "2.3.2"
sigio_ver=os.getenv("sfcio_ver") or "1.4.1"
sigio_ver=os.getenv("nemsio_ver") or "2.5.2"
sigio_ver=os.getenv("wrf_io_ver") or "1.2.0"
sigio_ver=os.getenv("ncio_ver") or "1.0.2"
sigio_ver=os.getenv("crtm_ver") or "2.3.0"

load(pathJoin("prod_util", prod_util_ver))

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

