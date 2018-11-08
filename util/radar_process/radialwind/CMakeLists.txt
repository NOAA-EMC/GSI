cmake_minimum_required(VERSION 2.6)
#  set(CMAKE_Fortran_LIB_DIRECTORY "${PROJECT_BINARY_DIR}/lib")
#
  file(GLOB LOCAL_SRC ${CMAKE_CURRENT_SOURCE_DIR}/*.f90)
  set_source_files_properties( ${LOCAL_SRC} PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )

  add_executable(bufr_decode_l2rwbufr.x  bufr_decode_l2rwbufr.f90 )
  set_target_properties( bufr_decode_l2rwbufr.x PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )
  target_link_libraries( bufr_decode_l2rwbufr.x ${BUFR_LIBRARY} ) 

  add_executable(bufr_encode_l2rwbufr.x bufr_encode_l2rwbufr.f90 )
  set_target_properties( bufr_encode_l2rwbufr.x PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )
  target_link_libraries( bufr_encode_l2rwbufr.x ${BUFR_LIBRARY} ) 

  add_executable(bufr_encode_radarbufr.x bufr_encode_radarbufr.f90 )
  set_target_properties( bufr_encode_radarbufr.x PROPERTIES COMPILE_FLAGS ${UTIL_COM_Fortran_FLAGS} )
  target_link_libraries( bufr_encode_radarbufr.x ${BUFR_LIBRARY} ) 
