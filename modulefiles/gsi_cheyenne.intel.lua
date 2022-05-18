help([[
]])

load("cmake/3.22.0")
load("python/3.7.9")
load("ncarenv/1.3")
load("intel/2021.2")
load("mpt/2.22")
load("ncarcompilers/0.5.0")

prepend_path("MODULEPATH", "/glade/p/ral/jntp/GMTB/tools/hpc-stack-v1.2.0/modulefiles/stack")

load("hpc/1.2.0")
load("hpc-intel/2021.2")
load("hpc-mpt/2.22")
load("mkl/2021.2")

load("gsi_common")

local prod_util_ver=os.getenv("prod_util_ver") or "1.2.2"
load(pathJoin("prod_util", prod_util_ver))

pushenv("CFLAGS", "-xHOST")
pushenv("FFLAGS", "-xHOST")

whatis("Description: GSI environment on Cheyenne with Intel Compilers")
