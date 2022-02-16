help([[
Load environment to compile GSI on Hera
]])

prepend_path("MODULEPATH", "/scratch2/NCEPDEV/nwprod/hpc-stack/libs/hpc-stack/modulefiles/stack")

hpc_ver=os.getenv("hpc_ver") or "1.1.0"
load(pathJoin("hpc", hpc_ver))

hpc_intel_ver=os.getenv("hpc_intel_ver") or "18.0.5.274"
load(pathJoin("hpc-intel", hpc_intel_ver))

impi_ver=os.getenv("hpc_impi_ver") or "2018.0.4"
load(pathJoin("hpc-impi", hpc_impi_ver))

cmake_ver=os.getenv("cmake_ver") or "3.16.1"
load(pathJoin("cmake", cmake_ver))

prepend_path("MODULEPATH", "/contrib/anaconda/modulefiles")

anaconda_ver=os.getenv("anaconda_ver") or "2.3.0"
load(pathJoin("anaconda", anaconda_ver))

prod_util_ver=os.getenv("prod_util_ver") or "1.2.2"
load(pathJoin("prod_util", prod_util_ver))

bufr_ver=os.getenv("bufr_ver") or "11.4.0"
load(pathJoin("bufr", bufr_ver))

ip_ver=os.getenv("ip_ver") or "3.3.3"
load(pathJoin("ip", ip_ver))

nemsio_ver=os.getenv("nemsio_ver") or "2.5.2"
load(pathJoin("nemsio", nemsio_ver))

sfcio_ver=os.getenv("sfcio_ver") or "1.4.1"
load(pathJoin("sfcio", sfcio_ver))

sigio_ver=os.getenv("sigio_ver") or "2.3.2"
load(pathJoin("sigio", sigio_ver))

sp_ver=os.getenv("sp_ver") or "2.3.3"
load(pathJoin("sp", sp_ver))

w3nco_ver=os.getenv("w3nco_ver") or "2.4.1"
load(pathJoin("w3nco", w3nco_ver))

w3emc_ver=os.getenv("w3emc_ver") or "2.7.3"
load(pathJoin("w3emc", w3emc_ver))

bacio_ver=os.getenv("bacio_ver") or "2.4.1"
load(pathJoin("bacio", bacio_ver))

crtm_ver=os.getenv("crtm_ver") or "2.3.0"
load(pathJoin("crtm", crtm_ver))

netcdf_ver=os.getenv("netcdf_ver") or "4.7.4"
load(pathJoin("netcdf", netcdf_ver))

whatis("Description: GSI build environment")
