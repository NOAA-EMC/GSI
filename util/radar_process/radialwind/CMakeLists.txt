cmake_minimum_required(VERSION 2.6)
#  set(CMAKE_Fortran_LIB_DIRECTORY "${PROJECT_BINARY_DIR}/lib")
#
  file(GLOB LOCAL_SRC ${CMAKE_CURRENT_SOURCE_DIR}/*.f90)
  set_source_files_properties( ${LOCAL_SRC} PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )

  add_executable(bufr::bufr_decode_l2rwbufr::bufr.x  bufr::bufr_decode_l2rwbufr::bufr.f90 )
  set_target_properties( bufr::bufr_decode_l2rwbufr::bufr.x PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )
  target_link_libraries( bufr::bufr_decode_l2rwbufr::bufr.x bufr::bufr_4 ) 

  add_executable(bufr::bufr_encode_l2rwbufr::bufr.x bufr::bufr_encode_l2rwbufr::bufr.f90 )
  set_target_properties( bufr::bufr_encode_l2rwbufr::bufr.x PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )
  target_link_libraries( bufr::bufr_encode_l2rwbufr::bufr.x bufr::bufr_4 ) 

  add_executable(bufr::bufr_encode_radarbufr::bufr.x bufr::bufr_encode_radarbufr::bufr.f90 )
  set_target_properties( bufr::bufr_encode_radarbufr::bufr.x PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )
  target_link_libraries( bufr::bufr_encode_radarbufr::bufr.x bufr::bufr_4 ) 
