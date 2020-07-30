cmake_minimum_required(VERSION 2.8)
if(BUILD_NCDIAG)
  if(BUILD_NCDIAG_SERIAL)
    add_subdirectory(serial)
  endif(BUILD_NCDIAG_SERIAL)
  set(Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include")

  # NetCDF-4 library
  include_directories( ${NETCDF_INCLUDES} ${NCDIAG_INCS} )

  # 32-bit reals, for now
  add_definitions(-D_REAL4_ -DUSE_MPI)
  FILE(GLOB NCDIAG_SRC ${CMAKE_CURRENT_SOURCE_DIR}/*90 )
  set_source_files_properties( ${NCDIAG_SRC} PROPERTIES COMPILE_FLAGS ${NCDIAG_Fortran_FLAGS} )
  LIST(REMOVE_ITEM NCDIAG_SRC ${CMAKE_CURRENT_SOURCE_DIR}/test_nc_unlimdims.F90 ) 
  LIST(REMOVE_ITEM NCDIAG_SRC ${CMAKE_CURRENT_SOURCE_DIR}/nc_diag_cat.F90 ) 
  add_library(ncdiag STATIC ${NCDIAG_SRC})
  add_executable(test_nc_unlimdims.x ${CMAKE_CURRENT_SOURCE_DIR}/test_nc_unlimdims.F90 )
  add_executable(nc_diag_cat.x ${CMAKE_CURRENT_SOURCE_DIR}/nc_diag_cat.F90 )
  target_link_libraries(nc_diag_cat.x ncdiag ${NETCDF_LIBRARIES} ${HDF5_HL_LIBRARIES} ${HDF5_LIBRARIES} ${ZLIB_LIBRARIES} ${MPI_Fortran_LIBRARIES})  
  target_link_libraries(test_nc_unlimdims.x ncdiag ${NETCDF_LIBRARIES} ${HDF5_HL_LIBRARIES} ${HDF5_LIBRARIES} ${ZLIB_LIBRARIES} ${MPI_Fortran_LIBRARIES})  
# set_target_properties(test_nc_unlimdims.x PROPERTIES Fortran_MODULE_DIRECTORY ${NCDIAG_INCS} )
# set_target_properties(nc_diag_cat.x PROPERTIES Fortran_MODULE_DIRECTORY ${NCDIAG_INCS} ${MPI_Fortran_INCLUDE_PATH})
 
endif(BUILD_NCDIAG)

