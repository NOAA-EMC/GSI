cmake_minimum_required(VERSION 2.6)
  file(GLOB CONMON_READ_PS_IG_SRC ${CMAKE_CURRENT_SOURCE_DIR}/*90 )
  set(CONMON_READ_PS_IG_Fortran_FLAGS "-fp-model strict -assume byterecl -convert big_endian -O3 -traceback -D_REAL8_ ")
  set_source_files_properties( ${CONMON_READ_PS_IG_SRC} PROPERTIES COMPILE_FLAGS ${CONMON_READ_PS_IG_Fortran_FLAGS} )
  add_executable(conmon_read_ps_IG.x  ${CONMON_READ_PS_IG_SRC} )

  set_target_properties( conmon_read_ps_IG.x PROPERTIES COMPILE_FLAGS ${CONMON_READ_PS_IG_Fortran_FLAGS} )
  include_directories( ${CORE_INCS} ${NCDIAG_INCS} ) 

  target_link_libraries( conmon_read_ps_IG.x ${W3NCO_4_LIBRARY} ${NCDIAG_LIBRARIES} ${NETCDF_Fortran_LIBRARIES} )
  if(BUILD_W3NCO)
    add_dependencies( conmon_read_ps_IG.x ${W3NCO_4_LIBRARY} )
  endif()


