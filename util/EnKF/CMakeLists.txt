# No utility code here depends on the GSI library
if(BUILD_UTIL_ENKF_GFS)
  add_subdirectory(gfs)
endif()

# Utility code here depends on the GSI library
# Specifically GSI_MODE=Regional
if(BUILD_UTIL_ENKF_ARW)
  if(NOT TARGET gsi)
    if(NOT gsi_FOUND)
      message(FATAL_ERROR "Utils: GSI library was not found for EnKF ARW utilities")
    endif()
  endif()
  if(NOT GSI_MODE MATCHES "Regional")
    message(FATAL_ERROR "Utils: Incompatible GSI = ${GSI_MODE} for EnKF ARW utilities")
  endif()
  add_subdirectory(arw)
endif()
