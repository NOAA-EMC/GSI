cmake_minimum_required(VERSION 2.6)
  file(GLOB LOCAL_SRC ${CMAKE_CURRENT_SOURCE_DIR}/*.f90)
  set_source_files_properties( ${LOCAL_SRC} PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )

  add_executable(read_diag_conv.x  read_diag_conv.f90 )
  set_target_properties( read_diag_conv.x PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )
  target_link_libraries( read_diag_conv.x ${GSISHAREDLIB} ${GSILIB} ${GSISHAREDLIB} )
  add_dependencies(read_diag_conv.x ${GSILIB} )

  add_executable(read_diag_rad.x  read_diag_rad.f90 )
  set_target_properties( read_diag_rad.x PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )
  target_link_libraries( read_diag_rad.x ${GSISHAREDLIB} ${GSILIB} ${GSISHAREDLIB} )
  add_dependencies(read_diag_rad.x ${GSILIB} )
