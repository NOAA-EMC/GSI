help([[
]])

load("cmake/3.22.0")
load("python/3.7.9")
load("ncarenv/1.3")
load("gnu/10.1.0")
load("mpt/2.22")
load("ncarcompilers/0.5.0")
unload("netcdf")

prepend_path("MODULEPATH", "/glade/work/epicufsrt/GMTB/tools/gnu/10.1.0/hpc-stack-v1.2.0/modulefiles/stack")

load("hpc/1.2.0")
load("hpc-gnu/10.1.0")
load("hpc-mpt/2.22")

-- Preload w3nco to work around nemsio "find_dependency(w3nco)" hpc-stack bug
load("w3nco/2.4.1")

load("gsi_common")

local prod_util_ver=os.getenv("prod_util_ver") or "1.2.2"
load(pathJoin("prod_util", prod_util_ver))

pushenv("MKLROOT", "/glade/u/apps/opt/intel/2022.1/mkl/latest")

pushenv("CC",  "mpicc")
pushenv("FC",  "mpif90")
pushenv("CXX", "mpicxx")

whatis("Description: GSI environment on Cheyenne with GNU Compilers")
