help([[
]])

load("cmake/3.22.0")
load("python/3.7.9")
load("ncarenv/1.3")
load("gnu/11.2.0")
load("mpt/2.25")
load("ncarcompilers/0.5.0")
unload("intel")
unload("netcdf")

prepend_path("MODULEPATH", "/glade/work/epicufsrt/contrib/hpc-stack/gnu11.2.0/modulefiles/stack")

load("hpc/1.2.0")
load("hpc-gnu/11.2.0")
load("hpc-mpt/2.25")

-- Preload w3nco to work around nemsio "find_dependency(w3nco)" hpc-stack bug
load("w3nco/2.4.1")

load("gsi_common")

load(pathJoin("prod_util", os.getenv("prod_util_ver") or "1.2.2"))
load(pathJoin("openblas", os.getenv("openblas_ver") or "0.3.23"))

pushenv("GSI_BINARY_SOURCE_DIR", "/glade/work/epicufsrt/contrib/GSI_fix/fix")

pushenv("CC",  "mpicc")
pushenv("FC",  "mpif90")
pushenv("CXX", "mpicxx")

whatis("Description: GSI environment on Cheyenne with GNU Compilers")
