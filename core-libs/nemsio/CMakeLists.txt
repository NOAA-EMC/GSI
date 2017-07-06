cmake_minimum_required(VERSION 2.6)
if(BUILD_CORELIBS)
  file(GLOB NEMSIO_SRC ${NEMSIO_DIR}/*.f90)
  set_source_files_properties( ${NEMSIO_SRC} COMPILE_FLAGS ${NEMSIO_Fortran_FLAGS} )
  add_library( ${nemsio} STATIC ${NEMSIO_SRC} )
  set_target_properties( ${nemsio} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_INCLUDE_OUTPUT_DIRECTORY} )
 
endif()
