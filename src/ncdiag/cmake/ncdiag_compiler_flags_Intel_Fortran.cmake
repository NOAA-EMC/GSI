####################################################################
# FLAGS COMMON TO ALL BUILD TYPES
####################################################################

set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -traceback -assume byterecl -convert big_endian -implicitnone")

####################################################################
# RELEASE FLAGS
####################################################################

set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -fp-model strict")

####################################################################
# DEBUG FLAGS
####################################################################

set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -check bounds -warn -heap-arrays -fpe-all=0 -fpe:0 -check all")

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
