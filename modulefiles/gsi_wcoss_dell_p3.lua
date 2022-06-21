help([[
]])

local hpc_ver=os.getenv("hpc_ver") or "1.1.0"
local hpc_ips_ver=os.getenv("hpc_ips_ver") or "18.0.1.163"
local hpc_impi_ver=os.getenv("hpc_impi_ver") or "18.0.1"
local cmake_ver=os.getenv("cmake_ver") or "3.20.0"
local lsf_ver=os.getenv("lsf_ver") or "10.1"
local jasper_ver=os.getenv("jasper_ver") or "2.0.22"
local zlib_ver=os.getenv("zlib_ver") or "1.2.11"
local png_ver=os.getenv("png_ver") or "1.6.35"
local python_ver=os.getenv("python_ver") or "3.6.3"
local prod_util_ver=os.getenv("prod_util_ver") or "1.2.2"

load(pathJoin("python", python_ver))
load(pathJoin("lsf", lsf_ver))

prepend_path("MODULEPATH", "/usrx/local/nceplibs/dev/hpc-stack/libs/hpc-stack/modulefiles/stack")

load(pathJoin("hpc", hpc_ver))
load(pathJoin("hpc-ips", hpc_ips_ver))
load(pathJoin("hpc-impi", hpc_impi_ver))

load(pathJoin("cmake", cmake_ver))

load(pathJoin("jasper", jasper_ver))
load(pathJoin("zlib", zlib_ver))
load(pathJoin("png", png_ver))

load("gsi_common")

load(pathJoin("prod_util", prod_util_ver))

pushenv("CFLAGS", "-xHOST")
pushenv("FFLAGS", "-xHOST")

whatis("Description: GSI environment on WCOSS Dell")
