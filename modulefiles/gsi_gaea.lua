help([[
  This module loads libraries required for building GSI
  on the NOAA RDHPC machine Gaea using Intel-2022.0.2
]])

whatis([===[Loads libraries needed for building GSI on Gaea ]===])

load_any(pathJoin("cmake", os.getenv("cmake_ver") or "3.20.1"),"cmake")

prepend_path("MODULEPATH","/lustre/f2/dev/role.epic/contrib/hpc-stack/intel-classic-2022.0.2/modulefiles/stack")

load(pathJoin("hpc", os.getenv("hpc_ver") or "1.2.0"))
load(pathJoin("intel-classic", os.getenv("intel_classic_ver") or "2022.0.2"))
load(pathJoin("cray-mpich", os.getenv("cray_mpich_ver") or "7.7.20"))
load(pathJoin("hpc-intel-classic", os.getenv("hpc_intel_classic_ver") or "2022.0.2"))
load(pathJoin("hpc-cray-mpich", os.getenv("hpc_cray_mpich_ver") or "7.7.20"))

local ufs_modules = {
  {["netcdf"]      = "4.7.4"},
  {["bacio"]       = "2.4.1"},
  {["crtm"]        = "2.4.0"},
  {["ip"]          = "3.3.3"},
  {["sp"]          = "2.3.3"},
  {["w3emc"]       = "2.9.2"},
  {["nemsio"]      = "2.5.4"},
  {["sigio"]       = "2.3.2"},
  {["sfcio"]       = "1.4.1"},
  {["wrf_io"]      = "1.2.0"},
  {["bufr"]        = "11.7.0"},
  {["nco"]         = "5.0.6"},
  {["cdo"]         = "1.9.8"},
  {["ncio"]        = "1.1.2"},
  {["ncdiag"]      = "1.1.1"},
  {["prod_util"]   = "1.2.2"},
}

for i = 1, #ufs_modules do
  for name, default_version in pairs(ufs_modules[i]) do
    local env_version_name = string.gsub(name, "-", "_") .. "_ver"
    load(pathJoin(name, os.getenv(env_version_name) or default_version))
  end
end

setenv("LD", "ftn")
setenv("CC", "cc")
setenv("CXX", "CC")
setenv("FC", "ftn")
setenv("CMAKE_C_COMPILER", "cc")
setenv("CMAKE_CXX_COMPILER", "CC")
setenv("CMAKE_Fortran_COMPILER", "ftn")
setenv("CMAKE_Platform", "gaea")

whatis("Description: GSI environment on GAEA with Intel Compilers")