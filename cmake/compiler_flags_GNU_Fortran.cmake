####################################################################
# FLAGS COMMON TO ALL BUILD TYPES
####################################################################

set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -fbacktrace")

####################################################################
# RELEASE FLAGS
####################################################################

set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -funroll-all-loops -finline-functions ")

####################################################################
# DEBUG FLAGS
####################################################################

set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -fcheck=bounds -ffpe-trap=invalid,zero,overflow")

####################################################################
# PRODUCTION FLAGS
####################################################################

set(CMAKE_Fortran_FLAGS_PRODUCTION "-O2 -funroll-all-loops -finline-functions")

####################################################################
# LINK FLAGS
####################################################################

set(CMAKE_Fortran_LINK_FLAGS "")

####################################################################
# FLAGS FOR AUTOPROFILING
####################################################################

set(Fortran_AUTOPROFILING_FLAGS "-finstrument-functions")

####################################################################

# Meaning of flags
# ----------------
# -fstack-arrays     : Allocate automatic arrays on the stack (needs large stacksize!!!)
# -funroll-all-loops : Unroll all loops
# -fcheck=bounds     : Bounds checking
