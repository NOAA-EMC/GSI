help([[
]])

load("cmake/3.20.1")

prepend_path("MODULEPATH","/lustre/f2/dev/role.epic/contrib/hpc-stack/intel-classic-2022.0.2/modulefiles/stack")
load(pathJoin("hpc", os.getenv("hpc_ver") or "1.2.0"))

load(pathJoin("intel-classic", os.getenv("intel_classic_ver") or "2022.0.2"))
load(pathJoin("cray-mpich", os.getenv("cray_mpich_ver") or "7.7.20"))
load(pathJoin("hpc-intel-classic", os.getenv("hpc_intel_classic_ver") or "2022.0.2"))
load(pathJoin("hpc-cray-mpich", os.getenv("hpc_cray_mpich_ver") or "7.7.20"))

load("gsi_common")

local prod_util_ver=os.getenv("prod_util_ver") or "1.2.2"
load(pathJoin("prod_util", prod_util_ver))

-- Needed at runtime:
load("alps")

local MKLROOT="/opt/intel/oneapi/mkl/2022.0.2/"
prepend_path("LD_LIBRARY_PATH",pathJoin(MKLROOT,"lib/intel64"))
pushenv("MKLROOT", MKLROOT)

pushenv("GSI_BINARY_SOURCE_DIR", "/lustre/f2/dev/role.epic/contrib/GSI_data/fix/20230601")
setenv("CC","cc")
setenv("FC","ftn")
setenv("CXX","CC")
pushenv("CRAYPE_LINK_TYPE","dynamic")

whatis("Description: GSI environment on Gaea with Intel Compilers")

