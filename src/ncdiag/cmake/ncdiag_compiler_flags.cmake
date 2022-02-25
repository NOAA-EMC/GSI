if(NOT CMAKE_BUILD_TYPE MATCHES "Debug")
  add_definitions(-DNDEBUG)
endif()

#######################################################################################
# Fortran
#######################################################################################

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  include(ncdiag_compiler_flags_GNU_Fortran)
elseif( CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  include(ncdiag_compiler_flags_Intel_Fortran)
else()
  message(STATUS "Fortran compiler with ID ${CMAKE_Fortran_COMPILER_ID} will be used with CMake default options")
endif()
