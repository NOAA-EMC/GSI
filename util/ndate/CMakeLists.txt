cmake_minimum_required(VERSION 2.6)
  set(NDATE_SRC ${CMAKE_CURRENT_SOURCE_DIR}/ndate.f ${W3NCO_DIR}/w3valdat.f ${W3NCO_DIR}/errexit.f ${W3NCO_DIR}/errmsg.f ${W3NCO_DIR}/w3reddat.f ${W3NCO_DIR}/iw3jdn.f ${W3NCO_DIR}/w3movdat.f ${W3NCO_DIR}/w3fs26.f  ${W3NCO_DIR}/w3utcdat.f )
 
# set(NDATE_Fortran_FLAGS "-fp-model source -ftz -assume byterecl -convert big_endian -heap-arrays  -DCOMMCODE -DLINUX -DUPPLITTLEENDIAN -O3 -Wl,-noinhibit-exec ")
  set_source_files_properties( ${NDATE_SRC} PROPERTIES COMPILE_FLAGS ${NDATE_Fortran_FLAGS} )
  add_executable(ndate.x  ${NDATE_SRC} )
  set_target_properties( ndate.x PROPERTIES COMPILE_FLAGS ${NDATE_Fortran_FLAGS} )
  include_directories( ${CORE_INCS} ) 
  target_link_libraries( ndate.x ${W3NCO_LIBRARY} )
  if(BUILD_W3NCO)
    add_dependencies( ndate.x ${W3NCO_LIBRARY} )
  endif()

