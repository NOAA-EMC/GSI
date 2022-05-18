help([[
]])

local PrgEnv_intel_ver=os.getenv("PrgEnv_intel_ver") or "8.1.0"
local intel_ver=os.getenv("intel_ver") or "19.1.3.304"
local craype_ver=os.getenv("craype_ver") or "2.7.8"
local cray_mpich_ver=os.getenv("cray_mpich_ver") or "8.1.7"
local cmake_ver= os.getenv("cmake_ver") or "3.20.2"
local python_ver=os.getenv("python_ver") or "3.8.6"
local prod_util_ver=os.getenv("prod_util_ver") or "2.0.10"

load(pathJoin("PrgEnv-intel", PrgEnv_intel_ver))
load(pathJoin("intel", intel_ver))
load(pathJoin("craype", craype_ver))
load(pathJoin("cray-mpich", cray_mpich_ver))
load(pathJoin("cmake", cmake_ver))
load(pathJoin("python", python_ver))

load(pathJoin("prod_util", prod_util_ver))

load("gsi_common")

whatis("Description: GSI environment on WCOSS2")
