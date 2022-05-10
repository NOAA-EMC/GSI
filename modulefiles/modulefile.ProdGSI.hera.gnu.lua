help([[
]])

prepend_path("MODULEPATH", "/scratch2/NCEPDEV/nwprod/hpc-stack/libs/hpc-stack/modulefiles/stack")

hpc_ver=os.getenv("hpc_ver") or "1.1.0"
load(pathJoin("hpc", hpc_ver))

hpc_intel_ver=os.getenv("hpc_gnu_ver") or "9.2.0"
load(pathJoin("hpc-gnu", hpc_gnu_ver))

impi_ver=os.getenv("hpc_mpich_ver") or "3.3.2"
load(pathJoin("hpc-mpich", hpc_mpich_ver))

cmake_ver=os.getenv("cmake_ver") or "3.20.1"
load(pathJoin("cmake", cmake_ver))

load("modulefile.ProdGSI.common")

pushenv("MKLROOT", "/apps/oneapi/mkl/2022.0.2")

whatis("Description: GSI environment on Hera with GNU Compilers")
