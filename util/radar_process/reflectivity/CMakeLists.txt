cmake_minimum_required(VERSION 2.6)
  file(GLOB LOCAL_SRC ${CMAKE_CURRENT_SOURCE_DIR}/*.f90)
  set_source_files_properties( ${LOCAL_SRC} PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )
  include_directories( ${NETCDF_INCLUDE_DIRS}  ${MPI_Fortran_INCLUDE_PATH} )

  add_executable(process_NSSL_mosaic.x ${LOCAL_SRC} )
  add_dependencies(process_NSSL_mosaic.x ${GSILIB} ${GSISHAREDLIB} )
  set_target_properties( process_NSSL_mosaic.x PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )
  target_link_libraries( process_NSSL_mosaic.x ${GSISHAREDLIB} ${BUFR_LIBRARY} ${NETCDF_Fortran_LIBRARIES} ${NETCDF_C_LIBRARIES} ${MPI_Fortran_LIBRARIES} ) 
