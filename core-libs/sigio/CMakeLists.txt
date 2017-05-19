cmake_minimum_required(VERSION 2.6)
if(BUILD_CORELIBS)
  file(GLOB SIGIO_SRC ${SIGIO_DIR}/*.f)
  set_source_files_properties( ${SIGIO_SRC} COMPILE_FLAGS ${SIGIO_Fortran_FLAGS} )
  add_library( ${sigio} STATIC ${SIGIO_SRC} )
  set_target_properties( ${sigio} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_INCLUDE_OUTPUT_DIRECTORY} )
 
endif()
