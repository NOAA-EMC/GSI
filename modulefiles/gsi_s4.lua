help([[
]])

local hpc_ver=os.getenv("hpc_ver") or "1.1.0"
local hpc_intel_ver=os.getenv("hpc_intel_ver") or "18.0.4"
local hpc_impi_ver=os.getenv("hpc_impi_ver") or "18.0.4"
local miniconda_ver=os.getenv("miniconda_ver") or "3.8-s4"
local prod_util_ver=os.getenv("prod_util_ver") or "1.2.2"

prepend_path("MODULEPATH", "/data/prod/hpc-stack/modulefiles/stack")

load("license_intel/S4")
load(pathJoin("hpc", hpc_ver))
load(pathJoin("hpc-intel", hpc_intel_ver))
load(pathJoin("hpc-impi", hpc_impi_ver))

load(pathJoin("miniconda", miniconda_ver))

load("gsi_common")

load(pathJoin("prod_util", prod_util_ver))

pushenv("CFLAGS", "-march=ivybridge")
pushenv("FFLAGS", "-march=ivybridge")

whatis("Description: GSI environment on S4 with Intel Compilers")
