####################################################################
# FLAGS COMMON TO ALL BUILD TYPES
####################################################################

set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS} -g -traceback -implicitnone")


# From setIntelFlags.cmake
# Production/Release
#set(UTIL_Fortran_FLAGS "-O3 ${HOST_FLAG} -warn all -implicitnone -traceback -fp-model strict -convert big_endian -DWRF -D_REAL8_ ${OpenMP_Fortran_FLAGS}" CACHE INTERNAL "")
#set(UTIL_COM_Fortran_FLAGS "-O3 -fp-model source -convert big_endian -assume byterecl -implicitnone" CACHE INTERNAL "")

# Debug
#set(UTIL_Fortran_FLAGS "-O0 ${HOST_FLAG} -warn all -implicitnone -traceback -g -debug full -fp-model strict -convert big_endian -D_REAL8_ ${OpenMP_Fortran_FLAGS}" CACHE INTERNAL "")
#set(UTIL_COM_Fortran_FLAGS "-O0 -warn all -implicitnone -traceback -g -debug full -fp-model strict -convert big_endian" CACHE INTERNAL "")

####################################################################
# RELEASE FLAGS
####################################################################

set(CMAKE_Fortran_FLAGS_RELEASE "-O3")

####################################################################
# DEBUG FLAGS
####################################################################

set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -check bounds -warn all -heap-arrays -fpe-all=0 -fpe:0 -check all -debug full -fp-model strict")

####################################################################
# PRODUCTION FLAGS
####################################################################

set(CMAKE_Fortran_FLAGS_PRODUCTION "-O3")

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
# todo
