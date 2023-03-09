help([[
]])

prepend_path("MODULEPATH", "/mnt/lfs4/HFIP/hfv3gfs/role.epic/hpc-stack/libs/intel-18.0.5.274/modulefiles/stack")

local hpc_ver=os.getenv("hpc_ver") or "1.2.0"
local hpc_intel_ver=os.getenv("hpc_intel_ver") or "18.0.5.274"
local hpc_impi_ver=os.getenv("hpc_impi_ver") or "2018.4.274"
local cmake_ver=os.getenv("cmake_ver") or "3.20.1"
local anaconda_ver=os.getenv("anaconda_ver") or "5.3.1"
local prod_util_ver=os.getenv("prod_util_ver") or "1.2.2"

load(pathJoin("hpc", hpc_ver))
load(pathJoin("hpc-intel", hpc_intel_ver))
load(pathJoin("hpc-impi", hpc_impi_ver))
load(pathJoin("cmake", cmake_ver))

prepend_path("MODULEPATH", "/contrib/anaconda/modulefiles")

load(pathJoin("anaconda", anaconda_ver))

load("gsi_common")

load(pathJoin("prod_util", prod_util_ver))

pushenv("CFLAGS", "-axSSE4.2,AVX,CORE-AVX2")
pushenv("FFLAGS", "-axSSE4.2,AVX,CORE-AVX2")


pushenv("GSI_BINARY_SOURCE_DIR", "/mnt/lfs4/HFIP/hfv3gfs/glopara/git/fv3gfs/fix/gsi/20221128")

whatis("Description: GSI environment on Jet with Intel Compilers")
