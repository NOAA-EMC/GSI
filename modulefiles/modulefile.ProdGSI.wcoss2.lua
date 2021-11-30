help([[
Load environment to run GSI on WCOSS2
]])

load(pathJoin("envvar", os.getenv("envvar_ver")))
load(pathJoin("PrgEnv-intel", os.getenv("PrgEnv_intel_ver")))
load(pathJoin("craype", os.getenv("craype_ver")))
load(pathJoin("intel", os.getenv("intel_ver")))
load(pathJoin("cray-mpich", os.getenv("cray_mpich_ver")))

load(pathJoin("bacio", os.getenv("bacio_ver")))
load(pathJoin("bufr", os.getenv("bufr_ver")))
load(pathJoin("crtm", os.getenv("crtm_ver")))
load(pathJoin("ip", os.getenv("ip_ver")))
load(pathJoin("nemsio", os.getenv("nemsio_ver")))
load(pathJoin("prod_util", os.getenv("prod_util_ver")))
load(pathJoin("sfcio", os.getenv("sfcio_ver")))
load(pathJoin("sigio", os.getenv("sigio_ver")))
load(pathJoin("sp", os.getenv("sp_ver")))
load(pathJoin("w3emc", os.getenv("w3emc_ver")))
load(pathJoin("w3nco", os.getenv("w3nco_ver")))
load(pathJoin("hdf5", os.getenv("hdf5_ver")))
load(pathJoin("netcdf", os.getenv("netcdf_ver")))

load(pathJoin("cmake", os.getenv("cmake_ver")))
load(pathJoin("python", os.getenv("python_ver")))

whatis("Description: GSI run environment")
