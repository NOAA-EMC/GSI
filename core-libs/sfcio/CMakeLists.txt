cmake_minimum_required(VERSION 2.6)

if(BUILD_CORELIBS)
  file(GLOB SFCIO_SRC ${SFCIO_DIR}/*.f90 ${SFCIO_DIR}/*.f )
  set_source_files_properties( ${SFCIO_SRC} COMPILE_FLAGS ${SFCIO_Fortran_FLAGS} )
  add_library( ${sfcio} STATIC ${SFCIO_SRC} )
  set_target_properties( ${sfcio} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_INCLUDE_OUTPUT_DIRECTORY} )
 
endif()
