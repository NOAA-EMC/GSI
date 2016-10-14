cmake_minimum_required(VERSION 2.8)
  set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")
# get a list of all the fortran source files
  file(GLOB GSI_SRC ${CMAKE_CURRENT_SOURCE_DIR}/*90 )
# get a list of all cplr files
  file(GLOB CLASS_SRC ${CMAKE_CURRENT_SOURCE_DIR}/class*90 )
# create a list of all corresponding stub files
  string(REGEX REPLACE "class" "stub" STUB_SRC "${CLASS_SRC}") 
# create a list of all corresponding class files
  string(REGEX REPLACE "class" "cplr" CPLR_SRC "${CLASS_SRC}") 

# these files use the cplr or stub modules, so they need to be compiled for all versions of GSI (WRF, no-WRF, etc)
  set(LINKED_SRC "${CMAKE_CURRENT_SOURCE_DIR}/gesinfo.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/get_gefs_ensperts_dualres.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/get_gefs_for_regional.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/read_guess.F90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/bicglanczos.F90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/bicg.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/set_crtm_cloudmod.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/crtm_interface.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/gsi_nemsio_mod.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/ensctl2model_ad.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/ensctl2model.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/ensctl2state.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/ensctl2state_ad.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/pcgsoi.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/en_perts_io.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/hybrid_ensemble_isotropic.F90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/write_all.F90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/glbsoi.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/setupaod.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/setuprad.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/general_read_nmmb.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/get_nmmb_ensperts.f90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/gsimod.F90" 
   "${CMAKE_CURRENT_SOURCE_DIR}/gsimain.f90")

# remove all stub, cplr and linked files from the rest of the source files
  list( REMOVE_ITEM GSI_SRC ${LINKED_SRC} )
  list( REMOVE_ITEM GSI_SRC ${STUB_SRC} )
  list( REMOVE_ITEM GSI_SRC ${CPLR_SRC} )

# The specific regional/WRF source files
  if(USE_WRF)
    set( REGIONAL_SRC ${CPLR_SRC} )
    list( APPEND REGIONAL_SRC ${LINKED_SRC} )
    set_source_files_properties( ${REGIONAL_SRC} PROPERTIES COMPILE_FLAGS ${GSI_Fortran_FLAGS} )
  endif()

# The non-WRF specific src files
  set( GLOBAL_SRC ${CPLR_SRC} )
  set( GLOBAL_SRC ${STUB_SRC} )
  list( APPEND GLOBAL_SRC ${LINKED_SRC} )
  list( APPEND GSI_SRC  "${CMAKE_CURRENT_SOURCE_DIR}/blockIO.c")

  set_source_files_properties( ${GSI_SRC} PROPERTIES COMPILE_FLAGS ${GSI_Fortran_FLAGS} )
  set_source_files_properties( ${GLOBAL_SRC} PROPERTIES COMPILE_FLAGS ${GSI_Fortran_FLAGS} )

  set_source_files_properties( ${CMAKE_CURRENT_SOURCE_DIR}/blockIO.c  PROPERTIES COMPILE_FLAGS ${GSI_CFLAGS} )

  include_directories( ${CORE_INCS} ${NETCDF_INCLUDES} ${MPI_Fortran_INCLUDE_PATH} "./" )
  if(USE_WRF) 
    add_library(gsilib_wrf${debug_suffix} STATIC ${REGIONAL_SRC} )
  endif()

  add_library(gsilib${debug_suffix} STATIC ${GLOBAL_SRC} )
  add_library(gsilib_shrd${debug_suffix} STATIC ${GSI_SRC} )

  target_link_libraries( gsilib_shrd${debug_suffix} gsilib${debug_suffix} ${CORE_LIBRARIES})
  target_link_libraries( gsilib${debug_suffix} ${CORE_LIBRARIES})

  if(USE_WRF) 
    set_target_properties(gsilib_wrf PROPERTIES Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include/wrf")
  endif()
  set_target_properties(gsilib PROPERTIES Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include")
  set_target_properties(gsilib_shrd PROPERTIES Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include")
  if(USE_WRF) 
    target_include_directories(gsilib_wrf PUBLIC "${PROJECT_BINARY_DIR}/include/wrf")
  endif()
  target_include_directories(gsilib PUBLIC "${PROJECT_BINARY_DIR}/include")
  target_include_directories(gsilib_shrd PUBLIC "${PROJECT_BINARY_DIR}/include")
# add_library(clib${debug_suffix} STATIC ${CMAKE_CURRENT_SOURCE_DIR}/blockIO.c )

  add_dependencies(gsilib${debug_suffix} gsilib_shrd${debug_suffix} )

  if(BUILD_CORELIBS )
    if(USE_WRF) 
      add_dependencies(gsilib_wrf${debug_suffix} MODS gsilib_shrd${debug_suffix} ${bacio} ${crtm} ${bufr} ${nemsio} ${sigio} ${sfcio} ${sp} ${w3emc} ${w3nco} )
    endif()
    add_dependencies(gsilib${debug_suffix} gsilib_shrd${debug_suffix} ${bacio} ${crtm} ${bufr} ${nemsio} ${sigio} ${sfcio} ${sp} ${w3emc} ${w3nco} )
    add_dependencies(gsilib_shrd${debug_suffix} ${bacio} ${crtm} ${bufr} ${nemsio} ${sigio} ${sfcio} ${sp} ${w3emc} ${w3nco} )
  endif()

  if(USE_WRF) 
    add_dependencies(gsilib_wrf${debug_suffix} MODS gsilib_shrd${debug_suffix} )
    add_executable(gsi_wrf.x ${CMAKE_CURRENT_SOURCE_DIR}/gsimain.f90 ${CMAKE_CURRENT_SOURCE_DIR}/gsimod.F90 )
  endif()

  add_executable(gsi.x ${CMAKE_CURRENT_SOURCE_DIR}/gsimain.f90 ${CMAKE_CURRENT_SOURCE_DIR}/gsimod.F90 )

  if(USE_WRF) 
    set_target_properties(gsi_wrf.x PROPERTIES Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include")
    set_target_properties(gsi_wrf.x PROPERTIES Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include/wrf")
  endif()

  set_target_properties(gsi.x PROPERTIES Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include")

  if(BUILD_CORELIBS )
    add_dependencies(gsi.x gsilib_shrd${debug_suffix} gsilib${debug_suffix} ${w3nco} ${bacio} ${crtm} ${bufr} ${nemsio} ${sigio} ${sfcio} ${sp} ${w3emc} )
    if(USE_WRF) 
      add_dependencies(gsi_wrf.x gsilib_shrd${debug_suffix} gsilib_wrf${debug_suffix} ${bacio} ${crtm} ${bufr} ${nemsio} ${sigio} ${sfcio} ${sp} ${w3emc} ${w3nco} )
    endif()
  else()
    add_dependencies(gsi.x gsilib_shrd${debug_suffix} gsilib${debug_suffix} )
    if(USE_WRF) 
      add_dependencies(gsi_wrf.x gsilib_shrd${debug_suffix} gsilib_wrf${debug_suffix} )
    endif()
  endif()

  list( REMOVE_ITEM LAPACK_LIBRARIES "-lm" )
  message("HEYEEEE wrf libs are ${WRF_LIBRARIES}")
  if(USE_WRF) 
    target_link_libraries(gsi_wrf.x gsilib_shrd${debug_suffix} gsilib_wrf${debug_suffix} gsilib_shrd${debug_suffix} ${WRF_LIBRARIES} ${CORE_LIBRARIES}
         ${NETCDF_LIBRARIES_F90} ${NETCDF_LIBRARIES} ${HDF5_Fortran_HL_LIBRARIES} ${MPI_Fortran_LIBRARIES} 
         ${LAPACK_LIBRARIES} -L./ ${EXTRA_LINKER_FLAGS} ${HDF5_LIBRARIES} ${CURL_LIBRARIES} )
  endif()
  target_link_libraries(gsi.x gsilib_shrd${debug_suffix} gsilib${debug_suffix} gsilib_shrd${debug_suffix}  
         ${NETCDF_LIBRARIES_F90} ${NETCDF_LIBRARIES} ${HDF5_Fortran_HL_LIBRARIES} ${MPI_Fortran_LIBRARIES}   
         ${LAPACK_LIBRARIES} -L./ ${EXTRA_LINKER_FLAGS} ${HDF5_LIBRARIES} ${CURL_LIBRARIES} ${CORE_LIBRARIES} )
