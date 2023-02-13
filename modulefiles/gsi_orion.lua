help([[
]])

prepend_path("MODULEPATH", "/work/noaa/da/role-da/spack-stack/modulefiles")

load("miniconda/3.9.7")
load("ecflow/5.8.4")

prepend_path("MODULEPATH", "/work2/noaa/da/dheinzel-new/spack-stack-unified-env-io-updates/envs/unified-dev-test2/install/modulefiles/Core")

load("stack-intel/2022.0.2")
load("stack-intel-oneapi-mpi/2021.5.1")
load("stack-python/3.9.7")
load("cmake/3.22.1")
load("sfcio/1.4.1")
load("crtm/2.4.0")
 
pushenv("CFLAGS", "-xHOST")
pushenv("FFLAGS", "-xHOST")

pushenv("GSI_BINARY_SOURCE_DIR", "/work/noaa/global/glopara/fix/gsi/20221128")

whatis("Description: GSI environment on Orion with Intel Compilers")
