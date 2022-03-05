if(NOT TARGET enkf)
  if(NOT enkf_FOUND)
    message(FATAL_ERROR "Utils: EnKF library was not found for EFSOI utilities")
  endif()
endif()

if(NOT ENKF_MODE MATCHES "GFS")
  message(FATAL_ERROR "Utils: Incompatible EnKF = ${ENKF_MODE} for EFSOI utilities")
endif()

add_subdirectory(src)
