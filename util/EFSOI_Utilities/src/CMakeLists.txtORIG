cmake_minimum_required(VERSION 2.6)
if(BUILD_EFSOI)
 # enable_language (Fortran)
 # set(Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include")
 # set(CMAKE_Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include")
 # set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")


  file(GLOB LOCAL_SRC ${CMAKE_CURRENT_SOURCE_DIR}/*90)

# set(Util_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include/efsoi")
  
  message(echo ${PROJECT_BINARY_DIR})
  
  set_source_files_properties( ${LOCAL_SRC} PROPERTIES COMPILE_FLAGS ${ENKF_Fortran_FLAGS} )
  
  add_executable(global_efsoi.x  ${LOCAL_SRC} )
  
  set_target_properties( global_efsoi.x PROPERTIES COMPILE_FLAGS ${ENKF_Fortran_FLAGS} )
  
# set_target_properties( global_efsoi.x PROPERTIES Fortran_MODULE_DIRECTORY ${Util_MODULE_DIRECTORY} )

  SET( CMAKE_EXE_LINKER_FLAGS  "${CMAKE_EXE_LINKER_FLAGS} ${OMPFLAG}" )
  
  include_directories( "${PROJECT_BINARY_DIR}/include/global" ${CMAKE_CURRENT_BINARY_DIR}${CORE_INCS} ${SIGIOINC} ${ENKFINC} ${FV3GFS_NCIO_INCS} ) 
  
  target_link_libraries( global_efsoi.x ${GSISHAREDLIB} ${GSILIB} enkflib enkfdeplib ${NCDIAG_LIBRARIES} ${LAPACK_LIBRARIES} ${GSILIB} ${GSISHAREDLIB} ${CORE_LIBRARIES} ${CORE_BUILT} ${NETCDF_LIBRARIES_F90} ${NETCDF_LIBRARIES}  ${MPI_Fortran_LIBRARIES} ${FV3GFS_NCIO_LIBRARIES} )
  
endif()
