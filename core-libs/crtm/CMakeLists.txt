cmake_minimum_required(VERSION 2.6)

if(BUILD_CORELIBS)
  file(GLOB CRTM_FPP_SRC ${CRTM_DIR}/*.fpp)
  set(CRTM_PP_SRC "")
  FOREACH( fppfile ${CRTM_FPP_SRC} )
    get_filename_component(SRC_WITHOUT_EXT ${fppfile} NAME_WE)
    set(F_SRC_FILE "${SRC_WITHOUT_EXT}.F")
   ADD_CUSTOM_COMMAND(
     OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/${F_SRC_FILE}
     PRE_BUILD
     COMMAND ${CMAKE_COMMAND} -E copy ${fppfile} ${CMAKE_CURRENT_SOURCE_DIR}/${F_SRC_FILE}
     DEPENDS ${fppfile}
   )
   list(APPEND CRTM_PP_SRC "${F_SRC_FILE}")
  endforeach()
  ADD_CUSTOM_TARGET(copy ALL DEPENDS ${CRTM_PP_SRC} )
  file(GLOB CRTM_SRC ${CRTM_DIR}/*.f90 )
  set_source_files_properties( ${CRTM_PP_SRC} COMPILE_FLAGS ${CRTM_Fortran_FLAGS} )
  set_source_files_properties( ${CRTM_SRC} COMPILE_FLAGS ${CRTM_Fortran_FLAGS} )
  
  set(CMAKE_Fortran_FLAGS_RELEASE "")
  add_library( ${crtm} STATIC ${CRTM_SRC} ${CRTM_PP_SRC} )
  set_target_properties( ${crtm} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_INCLUDE_OUTPUT_DIRECTORY} )
  include_directories( ${CRTM_DIR} )
endif()
