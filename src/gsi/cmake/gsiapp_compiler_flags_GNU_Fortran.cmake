####################################################################
# FLAGS COMMON TO ALL BUILD TYPES
####################################################################

set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -fno-second-underscore -ffast-math")

####################################################################
# RELEASE FLAGS
####################################################################

set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE}")

####################################################################
# DEBUG FLAGS
####################################################################

set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG}")

####################################################################
# PRODUCTION FLAGS
####################################################################

set(CMAKE_Fortran_FLAGS_PRODUCTION "${CMAKE_Fortran_FLAGS_PRODUCTION}")

####################################################################
# LINK FLAGS
####################################################################

set(CMAKE_Fortran_LINK_FLAGS "${CMAKE_Fortran_LINK_FLAGS}")

####################################################################
# FLAGS FOR AUTOPROFILING
####################################################################

set(Fortran_AUTOPROFILING_FLAGS "")

####################################################################

# Meaning of flags
# ----------------
# -fstack-arrays     : Allocate automatic arrays on the stack (needs large stacksize!!!)
# -funroll-all-loops : Unroll all loops
# -fcheck=bounds     : Bounds checking
