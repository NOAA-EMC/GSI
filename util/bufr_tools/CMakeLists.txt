cmake_minimum_required(VERSION 2.6)
#  set(CMAKE_Fortran_LIB_DIRECTORY "${PROJECT_BINARY_DIR}/lib")
#

  file(GLOB LOCAL_SRC ${CMAKE_CURRENT_SOURCE_DIR}/*.f90)
  set_source_files_properties( ${LOCAL_SRC} PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )

  add_executable(bufr::bufr_append_sample.x  bufr::bufr_append_sample.f90 )
  add_dependencies( bufr::bufr_append_sample.x bufr::bufr_4 )
  set_target_properties( bufr::bufr_append_sample.x PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )
  target_link_libraries( bufr::bufr_append_sample.x bufr::bufr_4 ) 

  add_executable(bufr::bufr_decode_sample.x  bufr::bufr_decode_sample.f90 )
  add_dependencies( bufr::bufr_decode_sample.x bufr::bufr_4 )
  set_target_properties( bufr::bufr_decode_sample.x PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )
  target_link_libraries( bufr::bufr_decode_sample.x bufr::bufr_4 ) 

  add_executable(bufr::bufr_encode_sample.x  bufr::bufr_encode_sample.f90 )
  add_dependencies( bufr::bufr_encode_sample.x bufr::bufr_4 )
  set_target_properties( bufr::bufr_encode_sample.x PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )
  target_link_libraries( bufr::bufr_encode_sample.x bufr::bufr_4 ) 

  add_executable(bufr::bufr_decode_radiance.x  bufr::bufr_decode_radiance.f90 )
  add_dependencies( bufr::bufr_decode_radiance.x bufr::bufr_4 )
  set_target_properties( bufr::bufr_decode_radiance.x PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )
  target_link_libraries( bufr::bufr_decode_radiance.x bufr::bufr_4 ) 

  add_executable(prepbufr::bufr_decode_all.x  prepbufr::bufr_decode_all.f90 )
  add_dependencies( prepbufr::bufr_decode_all.x bufr::bufr_4 )
  set_target_properties( prepbufr::bufr_decode_all.x PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )
  target_link_libraries( prepbufr::bufr_decode_all.x bufr::bufr_4 ) 

  add_executable(prepbufr::bufr_encode_surface.x  prepbufr::bufr_encode_surface.f90 )
  add_dependencies( prepbufr::bufr_encode_surface.x bufr::bufr_4 )
  set_target_properties( prepbufr::bufr_encode_surface.x PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )
  target_link_libraries( prepbufr::bufr_encode_surface.x bufr::bufr_4 ) 

  add_executable(prepbufr::bufr_encode_upperair.x  prepbufr::bufr_encode_upperair.f90 )
  add_dependencies( prepbufr::bufr_encode_upperair.x bufr::bufr_4 )
  set_target_properties( prepbufr::bufr_encode_upperair.x PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )
  target_link_libraries( prepbufr::bufr_encode_upperair.x bufr::bufr_4 ) 

  add_executable(prepbufr::bufr_inventory.x  prepbufr::bufr_inventory.f90 )
  add_dependencies( prepbufr::bufr_inventory.x bufr::bufr_4 )
  set_target_properties( prepbufr::bufr_inventory.x PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )
  target_link_libraries( prepbufr::bufr_inventory.x bufr::bufr_4 ) 

  add_executable(prepbufr::bufr_append_retrieve.x  prepbufr::bufr_append_retrieve.f90 )
  add_dependencies( prepbufr::bufr_append_retrieve.x bufr::bufr_4 )
  set_target_properties( prepbufr::bufr_append_retrieve.x PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )
  target_link_libraries( prepbufr::bufr_append_retrieve.x bufr::bufr_4 ) 

  add_executable(prepbufr::bufr_append_surface.x  prepbufr::bufr_append_surface.f90 )
  add_dependencies( prepbufr::bufr_append_surface.x bufr::bufr_4 )
  set_target_properties( prepbufr::bufr_append_surface.x PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )
  target_link_libraries( prepbufr::bufr_append_surface.x bufr::bufr_4 ) 

  add_executable(prepbufr::bufr_append_upperair.x  prepbufr::bufr_append_upperair.f90 )
  add_dependencies( prepbufr::bufr_append_upperair.x bufr::bufr_4 )
  set_target_properties( prepbufr::bufr_append_upperair.x PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )
  target_link_libraries( prepbufr::bufr_append_upperair.x bufr::bufr_4 ) 
