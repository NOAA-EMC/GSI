# These utilities require the Regional GSI to be compiled and available as a library.

if(NOT TARGET gsi)
  find_package(gsi QUIET)
endif()

if(NOT gsi_FOUND)
  message(STATUS "GSI library was not found. Disable EnKF ARW utilities")
  return()
endif()

if(NOT GSI_APP MATCHES "Regional")
  message(STATUS "Incompatible GSI = ${GSI_APP}, Disable EnKF ARW utilities")
  return()
endif()

if(NOT TARGET ncdiag)
  find_package(ncdiag REQUIRED)
endif()

add_subdirectory(enspreproc_regional.fd)
add_subdirectory(initialens_regional.fd)

