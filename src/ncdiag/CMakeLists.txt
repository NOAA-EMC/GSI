cmake_minimum_required(VERSION 2.8)
if(BUILD_NCDIAG)
  set(CMAKE_Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include")

  # NetCDF-4 library
# include(./cmake/FindBaselibs.cmake)
  include_directories( ${NETCDF_INCLUDES} ${NCDIAG_INCS} )

  # 32-bit reals, for now
# add_definitions(-D_REAL4_)
# add_definitions(-D_REAL8_)
  FILE(GLOB NCDIAG_SRC ${CMAKE_CURRENT_SOURCE_DIR}/*90 )
  LIST(APPEND NCDIAG_SRC "${CMAKE_CURRENT_SOURCE_DIR}/../kinds.F90")
  set_source_files_properties( ${NCDIAG_SRC} PROPERTIES COMPILE_FLAGS ${GSI_Fortran_FLAGS} )
  add_library(ncdiag STATIC ${NCDIAG_SRC})

endif(BUILD_NCDIAG)

