if(BUILD_GSDCLOUD)
  message(STATUS "Building GSD cloud analysis library")
  add_subdirectory(GSD)
endif()

if(BUILD_NCDIAG)
  message(STATUS "Building GSI NetCDF diagnostic library")
  add_subdirectory(ncdiag)
endif()

if(BUILD_GSI)
  message(STATUS "Building GSI")
  add_subdirectory(gsi)
endif()

if(BUILD_ENKF)
  message(STATUS "Building EnKF")
  add_subdirectory(enkf)
endif()
