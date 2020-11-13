cmake_minimum_required(VERSION 2.6)
  file(GLOB LOCAL_SRC ${CMAKE_CURRENT_SOURCE_DIR}/*.f90)
  set_source_files_properties( ${LOCAL_SRC} PROPERTIES COMPILE_FLAGS ${GSI_Fortran_FLAGS} )
  include_directories( "${PROJECT_BINARY_DIR}/include" ${NETCDF_INCLUDES}  ${MPI_Fortran_INCLUDE_PATH} )

  add_executable(initialens.x ${LOCAL_SRC} )
  set_target_properties( initialens.x PROPERTIES COMPILE_FLAGS ${GSI_Fortran_FLAGS} )
  target_link_libraries( initialens.x ${GSISHAREDLIB} ${GSILIB} ${GSISHAREDLIB} ${WRF_LIBRARIES} 
         ${NETCDF_LIBRARIES_F90} ${NETCDF_LIBRARIES} ${HDF5_HL_LIBRARIES} ${MPI_Fortran_LIBRARIES} 
         ${LAPACK_LIBRARIES} -L./ ${EXTRA_LINKER_FLAGS} ${HDF5_LIBRARIES} ${CURL_LIBRARIES} ${CORE_LIBRARIES} ${CORE_BUILT}
         ${GSI_LDFLAGS} ${NCDIAG_LIBRARIES} ${ZLIB_LIBRARIES} ${wrflib} )
  add_dependencies(initialens.x ${GSILIB})
