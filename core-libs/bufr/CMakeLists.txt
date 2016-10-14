cmake_minimum_required(VERSION 2.6)
#message("in bufr")
if(BUILD_CORELIBS)
  file(GLOB BUFR_F77_SRC ${BUFR_DIR}/*.f ${BUFR_DIR}/*.F)
  file(GLOB BUFR_C_SRC ${BUFR_DIR}/*.c)
  file(GLOB BUFR_PRM ${BUFR_DIR}/*.PRM)

  ADD_CUSTOM_COMMAND( OUTPUT "${CMAKE_INCLUDE_OUTPUT_DIRECTORY}/bufrlib.prm" 
     PRE_BUILD
     COMMAND /usr/bin/cpp -C -P -C -P -D_REAL8_ -DWRF -DLINUX -DPGI -traditional-cpp ${BUFR_DIR}/bufrlib.PRM -o ${CMAKE_INCLUDE_OUTPUT_DIRECTORY}/bufrlib.prm
     DEPENDS ${BUFR_DIR}/bufrlib.PRM
     )
  add_custom_target(bufrlib_prm DEPENDS ${CMAKE_INCLUDE_OUTPUT_DIRECTORY}/bufrlib.prm )
  if( BUFR_F77_SRC )
    set_source_files_properties( ${BUFR_F77_SRC} COMPILE_FLAGS ${BUFR_Fortran_FLAGS})
  endif()
  set_source_files_properties( ${BUFR_C_SRC} COMPILE_FLAGS ${BUFR_C_FLAGS} )
 
  add_library( ${bufr} STATIC ${BUFR_C_SRC} ${BUFR_F77_SRC} )
  add_dependencies(${bufr} bufrlib_prm)
  set_target_properties( ${bufr} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_INCLUDE_OUTPUT_DIRECTORY} )
endif()
