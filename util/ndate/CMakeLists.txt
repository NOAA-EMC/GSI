cmake_minimum_required(VERSION 2.6)

  set( NDATE_SRC ${CMAKE_CURRENT_SOURCE_DIR}/ndate.f )
  set_source_files_properties( ${NDATE_SRC} PROPERTIES COMPILE_FLAGS ${NDATE_Fortran_FLAGS} )

  add_executable( ndate.x  ${NDATE_SRC} )
  add_dependencies( ndate.x ${W3NCO_4_LIBRARY} )
  target_link_libraries( ndate.x ${W3NCO_4_LIBRARY} )

