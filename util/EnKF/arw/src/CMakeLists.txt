cmake_minimum_required(VERSION 2.6)

  set(CMAKE_Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/util/include")
  set(UTIL_INC ${CMAKE_Fortran_MODULE_DIRECTORY})

  add_subdirectory(enspreproc_regional.fd)
  add_subdirectory(initialens_regional.fd)

