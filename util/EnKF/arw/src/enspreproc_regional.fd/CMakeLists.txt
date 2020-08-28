cmake_minimum_required(VERSION 2.6)
  set(GSI_Fortran_FLAGS_LOCAL "${GSI_Fortran_FLAGS} -DWRF")
  file(GLOB LOCAL_SRC ${CMAKE_CURRENT_SOURCE_DIR}/*.f90 ${CMAKE_CURRENT_SOURCE_DIR}/*.F90)
  set_source_files_properties( ${LOCAL_SRC} PROPERTIES COMPILE_FLAGS ${GSI_Fortran_FLAGS_LOCAL} )
  include_directories( ${PROJECT_BINARY_DIR}/include ${CORE_INCS} ${NETCDF_INCLUDES}  ${MPI_Fortran_INCLUDE_PATH} ${FV3GFS_NCIO_INCS}  )

  add_executable(enspreproc.x ${LOCAL_SRC} )
  set_target_properties( enspreproc.x PROPERTIES COMPILE_FLAGS ${GSI_Fortran_FLAGS_LOCAL} )
  target_link_libraries(enspreproc.x ${GSISHAREDLIB} ${GSILIB} ${GSISHAREDLIB} ${WRF_LIBRARIES} 
         ${NETCDF_LIBRARIES_F90} ${NETCDF_LIBRARIES} ${HDF5_HL_LIBRARIES} ${MPI_Fortran_LIBRARIES} 
         ${LAPACK_LIBRARIES} -L./ ${EXTRA_LINKER_FLAGS} ${HDF5_LIBRARIES} ${CURL_LIBRARIES} ${CORE_LIBRARIES} ${CORE_BUILT}
         ${GSI_LDFLAGS} ${NCDIAG_LIBRARIES} ${ZLIB_LIBRARIES} ${wrflib} ${FV3GFS_NCIO_LIBRARIES} )
  add_dependencies(enspreproc.x ${GSILIB})
