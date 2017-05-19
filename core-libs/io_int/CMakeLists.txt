cmake_minimum_required(VERSION 2.6)
if(BUILD_CORELIBS)
  file(GLOB IO_INT_SRC *.f90)
  add_library( io_int${libsuffix} STATIC ${IO_INT_SRC} )
  set_target_properties( io_int${libsuffix} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_INCLUDE_OUTPUT_DIRECTORY} )
 
endif()
