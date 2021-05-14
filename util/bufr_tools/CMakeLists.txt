cmake_minimum_required(VERSION 2.6)
#  set(CMAKE_Fortran_LIB_DIRECTORY "${PROJECT_BINARY_DIR}/lib")
#

  file(GLOB LOCAL_SRC ${CMAKE_CURRENT_SOURCE_DIR}/*.f90)
  set_source_files_properties( ${LOCAL_SRC} PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )

  add_executable(bufr_append_sample.x  bufr_append_sample.f90 )
  set_target_properties( bufr_append_sample.x PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )
  target_link_libraries( bufr_append_sample.x ${BUFR_LIBRARY} ) 

  add_executable(bufr_decode_sample.x  bufr_decode_sample.f90 )
  set_target_properties( bufr_decode_sample.x PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )
  target_link_libraries( bufr_decode_sample.x ${BUFR_LIBRARY} ) 

  add_executable(bufr_encode_sample.x  bufr_encode_sample.f90 )
  set_target_properties( bufr_encode_sample.x PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )
  target_link_libraries( bufr_encode_sample.x ${BUFR_LIBRARY} ) 

  add_executable(bufr_decode_radiance.x  bufr_decode_radiance.f90 )
  set_target_properties( bufr_decode_radiance.x PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )
  target_link_libraries( bufr_decode_radiance.x ${BUFR_LIBRARY} ) 

  add_executable(prepbufr_decode_all.x  prepbufr_decode_all.f90 )
  set_target_properties( prepbufr_decode_all.x PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )
  target_link_libraries( prepbufr_decode_all.x ${BUFR_LIBRARY} ) 

  add_executable(prepbufr_encode_surface.x  prepbufr_encode_surface.f90 )
  set_target_properties( prepbufr_encode_surface.x PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )
  target_link_libraries( prepbufr_encode_surface.x ${BUFR_LIBRARY} ) 

  add_executable(prepbufr_encode_upperair.x  prepbufr_encode_upperair.f90 )
  set_target_properties( prepbufr_encode_upperair.x PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )
  target_link_libraries( prepbufr_encode_upperair.x ${BUFR_LIBRARY} ) 

  add_executable(prepbufr_inventory.x  prepbufr_inventory.f90 )
  set_target_properties( prepbufr_inventory.x PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )
  target_link_libraries( prepbufr_inventory.x ${BUFR_LIBRARY} ) 

  add_executable(prepbufr_append_retrieve.x  prepbufr_append_retrieve.f90 )
  set_target_properties( prepbufr_append_retrieve.x PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )
  target_link_libraries( prepbufr_append_retrieve.x ${BUFR_LIBRARY} ) 

  add_executable(prepbufr_append_surface.x  prepbufr_append_surface.f90 )
  set_target_properties( prepbufr_append_surface.x PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )
  target_link_libraries( prepbufr_append_surface.x ${BUFR_LIBRARY} ) 

  add_executable(prepbufr_append_upperair.x  prepbufr_append_upperair.f90 )
  set_target_properties( prepbufr_append_upperair.x PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )
  target_link_libraries( prepbufr_append_upperair.x ${BUFR_LIBRARY} ) 
