help([[
]])

prepend_path("MODULEPATH", "/scratch1/NCEPDEV/jcsda/jedipara/spack-stack/modulefiles")
prepend_path("MODULEPATH", "/scratch1/NCEPDEV/stmp4/Cameron.Book/dave_gsi/envs/gsi_test/install/modulefiles/Core")

local spack_python_ver=os.getenv("spack_python_ver") or "3.9.12"
local spack_intel_ver=os.getenv("spack_intel_ver") or "2021.5.0"
local spack_impi_ver=os.getenv("spack_impi_ver") or "2021.5.1"
local spack_python_ver=os.getenv("spack_impi_ver") or "3.9.12"
local ecflow_ver=os.getenv("ecflow_ver") or "5.5.3"
local mysql_ver=os.getenv("mysql_ver") or "8.0.31"

load(pathJoin("miniconda", spack_python_ver))
load(pathJoin("ecflow", ecflow_ver))
load(pathJoin("mysql", mysql_ver))
load(pathJoin("stack-intel", spack_intel_ver))
load(pathJoin("stack-intel-oneapi-mpi", spack_impi_ver))
load(pathJoin("stack-python", spack_python_ver))

load("ufs-srw-app-env/unified-dev")
load("gsi-ncdiag/1.1.1")
load("ncio/1.1.2")
load("bufr/12.0.0")

pushenv("CFLAGS", "-xHOST")
pushenv("FFLAGS", "-xHOST")

pushenv("GSI_BINARY_SOURCE_DIR", "/scratch1/NCEPDEV/global/glopara/fix/gsi/20230601")

whatis("Description: GSI environment on Hera with Intel Compilers")
