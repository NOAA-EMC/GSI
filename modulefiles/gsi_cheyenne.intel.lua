help([[
]])

load("cmake/3.22.0")
load("python/3.7.9")
load("ncarenv/1.3")
load("intel/2022.1")
load("mpt/2.25")
load("ncarcompilers/0.5.0")

prepend_path("MODULEPATH", "/glade/work/epicufsrt/GMTB/tools/intel/2022.1/hpc-stack-v1.2.0_6eb6/modulefiles/stack")

load("hpc/1.2.0")
load("hpc-intel/2022.1")
load("hpc-mpt/2.25")
load("mkl/2022.1")

load("gsi_common")

local prod_util_ver=os.getenv("prod_util_ver") or "1.2.2"
load(pathJoin("prod_util", prod_util_ver))

pushenv("CFLAGS", "-xHOST")
pushenv("FFLAGS", "-xHOST")

whatis("Description: GSI environment on Cheyenne with Intel Compilers")
