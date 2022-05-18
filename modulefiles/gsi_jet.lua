help([[
]])

load("cmake/3.20.1")

prepend_path("MODULEPATH", "/contrib/anaconda/modulefiles")

load("anaconda/5.3.1")

prepend_path("MODULEPATH", "/lfs4/HFIP/hfv3gfs/nwprod/hpc-stack/libs/modulefiles/stack")

load("hpc/1.1.0")
load("hpc-intel/18.0.5.274")
load("hpc-impi/2018.4.274")

load("gsi_common")

local prod_util_ver=os.getenv("prod_util_ver") or "1.2.2"
load(pathJoin("prod_util", prod_util_ver))

pushenv("CFLAGS", "-axSSE4.2,AVX,CORE-AVX2")
pushenv("FFLAGS", "-axSSE4.2,AVX,CORE-AVX2")

whatis("Description: GSI environment on Jet with Intel Compilers")
