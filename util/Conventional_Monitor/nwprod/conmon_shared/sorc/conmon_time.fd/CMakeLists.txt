cmake_minimum_required(VERSION 2.6)
  file(GLOB CONMON_TIME_SRC ${CMAKE_CURRENT_SOURCE_DIR}/*90 )
  set(CONMON_TIME_Fortran_FLAGS "-fp-model strict -assume byterecl -convert big_endian -O3 -traceback -D_REAL8_ ")
  set(Util_MODULE_DIR ${PROJECT_BINARY_DIR}/include/conmon_time )

  set_source_files_properties( ${CONMON_TIME_SRC} PROPERTIES COMPILE_FLAGS ${CONMON_TIME_Fortran_FLAGS} )
  add_executable(conmon_time.x  ${CONMON_TIME_SRC} )
  set_target_properties( conmon_time.x PROPERTIES COMPILE_FLAGS ${CONMON_TIME_Fortran_FLAGS} )
  set_target_properties( conmon_time.x PROPERTIES Fortran_MODULE_DIRECTORY ${Util_MODULE_DIR} )

  include_directories( ${CORE_INCS} ${NCDIAG_INCS} ) 
 
  target_link_libraries( conmon_time.x ${W3NCO_4_LIBRARY} ${NCDIAG_LIBRARIES} ${NETCDF_Fortran_LIBRARIES} 
     ${NETCDF_C_LIBRARIES} ${MPI_Fortran_LIBRARIES} )

  if(BUILD_W3NCO)
    add_dependencies( conmon_time.x ${W3NCO_4_LIBRARY} )
  endif()

