####################################################################
# FLAGS COMMON TO ALL BUILD TYPES
####################################################################

set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -fbacktrace")

if(${CMAKE_Fortran_COMPILER_VERSION} VERSION_GREATER_EQUAL 10)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fallow-argument-mismatch -fallow-invalid-boz")
endif()

# From setGNUFlags.cmake
# RELEASE OR PRODUCTION
#set(UTIL_Fortran_FLAGS     " -O3 -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -static -Wall -fno-range-check -DWRF -D_REAL8_ -fopenmp" CACHE INTERNAL "")
#set(UTIL_COM_Fortran_FLAGS " -O3 -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -ggdb -static -Wall -fno-range-check" CACHE INTERNAL "")

# DEBUG
#set(UTIL_Fortran_FLAGS     " -g -fbacktrace -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -g -ggdb -static -Wall -fno-range-check -DWRF -D_REAL8_ -fopenmp" CACHE INTERNAL "")
#set(UTIL_COM_Fortran_FLAGS " -g -fbacktrace -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4 -funroll-loops -g -ggdb -static -Wall -fno-range-check" CACHE INTERNAL "")

####################################################################
# RELEASE FLAGS
####################################################################

set(CMAKE_Fortran_FLAGS_RELEASE "-O3")

####################################################################
# DEBUG FLAGS
####################################################################

set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -ggdb -static -Wall -fcheck=bounds -ffpe-trap=invalid,zero,overflow")

####################################################################
# PRODUCTION FLAGS
####################################################################

set(CMAKE_Fortran_FLAGS_PRODUCTION "-O3 -funroll-all-loops -finline-functions")

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
