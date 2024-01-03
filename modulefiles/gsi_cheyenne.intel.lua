help([[
]])

unload("ncarenv/1.3")
unload("intel/19.1.1")
unload("ncarcompilers/0.5.0")
unload("mpt/2.25")
unload("netcdf/4.8.1")

prepend_path("MODULEPATH", "/glade/work/epicufsrt/contrib/spack-stack/cheyenne/spack-stack-1.4.1/envs/unified-env/install/modulefiles/Core")
prepend_path("MODULEPATH", "/glade/work/jedipara/cheyenne/spack-stack/modulefiles/misc")

local stack_python_ver=os.getenv("stack_python_ver") or "3.9.12"
local stack_intel_ver=os.getenv("stack_intel_ver") or "19.1.1.217"
local stack_mpi_ver=os.getenv("stack_mpi_ver") or "2019.7.217"
local cmake_ver=os.getenv("cmake_ver") or "3.22.0"

load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("stack-intel-mpi", stack_mpi_ver))
load(pathJoin("stack-python", stack_python_ver))
load(pathJoin("cmake", cmake_ver))

load("gsi_common")
load(pathJoin("prod-util", os.getenv("prod_util_ver") or "1.2.2"))
pushenv("GSI_BINARY_SOURCE_DIR", "/glade/work/epicufsrt/contrib/GSI_data/fix/20230911")

pushenv("CFLAGS", "-xHOST")
pushenv("FFLAGS", "-xHOST")

pushenv("GSI_BINARY_SOURCE_DIR", "/glade/work/epicufsrt/contrib/GSI_data/fix/20230601")

whatis("Description: GSI environment on Cheyenne with Intel Compilers")
