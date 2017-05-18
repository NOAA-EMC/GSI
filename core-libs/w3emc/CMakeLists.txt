cmake_minimum_required(VERSION 2.8)
if(BUILD_CORELIBS)
  file(GLOB W3EMC_SRC ${W3EMC_DIR}/*.F)
  file(GLOB W3EMC_F77_SRC ${W3EMC_DIR}/*.f)
  set_source_files_properties( ${W3EMC_F77_SRC} COMPILE_FLAGS ${W3EMC_Fortran_FLAGS})
  set_source_files_properties( ${W3EMC_SRC} COMPILE_FLAGS ${W3EMC_Fortran_FLAGS})
  include_directories(${SIGIOINC})
  add_library( ${w3emc} STATIC ${W3EMC_SRC} ${W3EMC_F77_SRC} )
  set_target_properties( ${w3emc} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_INCLUDE_OUTPUT_DIRECTORY} )

  add_dependencies(${w3emc} ${sigio} )

endif()
